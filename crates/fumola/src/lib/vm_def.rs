use crate::ast::{
    Dec, Delim, Exp, ExpField, ExpField_, Exp_, Id, Id_, Mut, Pat, Source, Stab_, Vis_,
};
use crate::shared::{FastClone, Share};
use crate::value::{ActorId, Closed, ClosedFunction, Value, Value_};
use crate::vm_types::{
    def::{
        Actor as ActorDef, Ctx, CtxId, Def, Defs, Field as FieldDef, Function as FunctionDef,
        Module as ModuleDef, Var as VarDef,
    },
    Active, ActiveBorrow, Cont, Core, Interruption, Limits, LocalPointer, ModuleFile,
    ModuleFileState, ModulePath, NamedPointer, Pointer, ScheduleChoice,
};
use im_rc::{HashMap, Vector};
use std::vec::Vec;

use crate::nyi;

impl Def {
    pub fn source(&self) -> Source {
        // to do
        Source::Evaluation
    }
}

impl CtxId {
    pub fn get_field<'a, A: ActiveBorrow>(&self, a: &'a A, id: &Id) -> Option<&'a FieldDef> {
        a.defs().map.get(self).unwrap().fields.get(id)
    }
}

impl Defs {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        let root = Ctx {
            parent: None,
            fields: HashMap::new(),
        };
        map.insert(CtxId(0), root);
        Defs {
            map,
            active_ctx: CtxId(0),
            next_ctx_id: 1,
            active_path: None,
        }
    }
    pub fn active_context(&self) -> CtxId {
        self.active_ctx.clone()
    }
    pub fn enter_context(&mut self, is_a_root: bool) -> (CtxId, CtxId) {
        let x = self.next_ctx_id;
        self.next_ctx_id = self
            .next_ctx_id
            .checked_add(1)
            .expect("Out of def-context ids.");
        let ctx = Ctx {
            parent: if is_a_root {
                None
            } else {
                Some(self.active_ctx.clone())
            },
            fields: HashMap::new(),
        };
        let prev = self.map.insert(CtxId(x), ctx);
        assert_eq!(prev, None);
        let saved = self.active_ctx.clone();
        self.active_ctx = CtxId(x);
        (saved, CtxId(x))
    }
    pub fn reenter_context(&mut self, x: &CtxId) -> (CtxId, CtxId, Ctx) {
        let old_ctx = self.map.get(x).unwrap().clone();
        let new_ctx = Ctx {
            parent: old_ctx.parent.clone(),
            fields: HashMap::new(),
        };
        let saved = self.active_ctx.clone();
        self.active_ctx = x.clone();
        self.map.insert(x.clone(), new_ctx);
        (saved, x.clone(), old_ctx)
    }
    pub fn insert_field(
        &mut self,
        i: &Id,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        def: Def,
    ) -> Result<(), Interruption> {
        let s = source.clone();
        let a = self.active_ctx.clone();
        let y = self.map.get_mut(&a).unwrap().fields.insert(
            i.clone(),
            crate::vm_types::def::Field {
                source,
                stab,
                vis,
                def,
            },
        );
        if let Some(y) = y {
            // to do -- both source infos for the error.
            Err(Interruption::AmbiguousIdentifer(
                i.clone(),
                s,
                y.source().clone(),
            ))
        } else {
            Ok(())
        }
    }
    // special-case: for named modules and actors whose new defs over-write prior ones.
    pub fn reinsert_field(
        &mut self,
        i: &Id,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        def: Def,
    ) -> Result<(), Interruption> {
        let a = self.active_ctx.clone();
        let y = self.map.get_mut(&a).unwrap().fields.insert(
            i.clone(),
            crate::vm_types::def::Field {
                source,
                stab,
                vis,
                def,
            },
        );
        if let Some(_) = y {
            Ok(())
        } else {
            unreachable!()
        }
    }

    pub fn leave_context(&mut self, saved: CtxId, sanity_check_active: &CtxId) {
        assert_eq!(&self.active_ctx, sanity_check_active);
        if let Some(parent) = self
            .map
            .get(&self.active_ctx)
            .expect("leave context")
            .parent
            .as_ref()
        {
            assert_eq!(parent, &saved)
        }
        self.active_ctx = saved;
    }
    pub fn report_diff(&self, _old_ctx: &Ctx) {
        // to do
        //
        // compare entries in current context to that of given old context param.
        // - for entries in both, the current context is the upgraded version, or unchanged version.
        // - (detecting change vs upgrade requires comparing but ingoring source locations, which can shift).
        // - for entries in only old context, the new context is deleting them.
        // - for entries in only the new context, the new context is adding it.
    }
    pub fn releave_context(&mut self, saved: CtxId, sanity_check_active: &CtxId, old_ctx: &Ctx) {
        self.report_diff(old_ctx);
        self.leave_context(saved, sanity_check_active)
    }
}

pub fn module_project(
    defs: &Defs,
    m: &ModuleDef,
    pattern: &Pat,
) -> Result<Vec<(Id_, Def)>, Interruption> {
    match pattern {
        Pat::Var(x) => Ok(vec![(x.clone(), Def::Module(m.clone()))]),
        Pat::Object(pat_fields) => {
            let mut r = vec![];
            for f in pat_fields.vec.iter() {
                match f.0.pat.clone() {
                    None => {
                        let fd = resolve_def(defs, &m.fields, true, f.0.id.0.id_ref())?;
                        r.push((f.0.id.0.id_(), fd.def.clone()))
                    }
                    Some(Pat::Var(x)) => {
                        let fd = resolve_def(defs, &m.fields, true, f.0.id.0.id_ref())?;
                        r.push((x.clone(), fd.def.clone()))
                    }
                    p => return nyi!(line!(), "module_project object-field pattern {:?}", p),
                }
            }
            Ok(r)
        }
        pattern => nyi!(line!(), "module_project ({:?}, {:?})", m, pattern),
    }
}

fn path_base(path: &String) -> String {
    let mut output = String::from("");
    let split = path.split("/");
    let parts = split.clone().count(); // probably a better way someday.
    let mut count = 0;
    for part in split {
        if count > 0 && count < parts - 1 {
            output = format!("{}/{}", output, part);
            count += 1;
        } else if count == 0 && count < parts - 1 {
            output = format!("{}", part);
            count += 1;
        } else {
            assert_eq!(count, parts - 1)
            // skip end of path, after all '/'
        }
    }
    output
}

pub mod def {
    use log::{debug, info, warn};

    use super::*;
    use crate::{
        ast::{DecField, DecFields},
        format::format_pretty,
        value::{Symbol, Text},
        Shared,
    };

    pub fn import<A: Active>(active: &mut A, path: &str) -> Result<ModuleDef, Interruption> {
        let path0 = path; // for log.
        let path = format!("{}", &path[1..path.len() - 1]);
        let (package_name, local_path) = if path == "mo:⛔" || path == "mo:prim" {
            (Some("⛔".to_string()), "lib".to_string())
        } else if path.starts_with("mo:") {
            let path = format!("{}", &path[3..path.len()]);
            let mut sep_parts = path.split("/");
            if let Some(package_name) = sep_parts.next() {
                if let Some(_) = sep_parts.next() {
                    let local_path = format!("{}", &path[package_name.len() + 1..path.len()]);
                    (Some(package_name.to_string()), local_path)
                } else {
                    (Some(package_name.to_string()), "lib".to_string())
                }
            } else {
                return nyi!(line!(), "import {}", path);
            }
        } else {
            let path = match active.defs().active_path.as_ref() {
                // we are "active" at some other path that's relative to this one.
                // we need to account for that.
                // e.g., if active_local_path = "foo/bar/baz/"
                // and if path = "foo.fumola"
                // then the local_path is "foo/bar/baz/foo.fumola"
                None => path,
                Some(prefix) => {
                    let mut prefix = prefix.clone();
                    if prefix.len() > 0 {
                        prefix.push_str("/");
                    };
                    prefix.push_str(path.as_str());
                    prefix
                }
            };
            (active.package().clone(), path)
        };
        let path = crate::vm_types::ModulePath {
            package_name: package_name.clone(),
            local_path: local_path.clone(),
        };
        log::debug!(
            "`import {}` resolves as `import {:?}`.  Attemping to import...",
            path0,
            path
        );
        let mf = active.module_files().map.get(&path).map(|x| x.clone());
        let mf = match mf {
            None => return Err(Interruption::ModuleFileNotFound(path)),
            Some(ModuleFileState::Defined(mf)) => mf,
            Some(ModuleFileState::Init(init)) => {
                // if a module imports itself, directly or indirectly, cycle detection prevents .
                // so, detect a cycle by tracking import paths.
                let contains_this_path = active.module_files().import_stack.contains(&path);
                if contains_this_path {
                    let mut stack = active.module_files().import_stack.clone();
                    stack.push_back(path.clone());
                    return Err(Interruption::ImportCycle(stack));
                } else {
                    active.defs().active_path = Some(path_base(&local_path));
                    active.module_files().import_stack.push_back(path.clone());
                };
                let importing_package = active.package().clone();
                *active.package() = package_name;
                let (saved, ctxid) = active.defs().enter_context(true);
                for dec in init.outer_decs.iter() {
                    let dec = dec.clone();
                    let df = crate::ast::DecField {
                        attrs: None,
                        vis: None,
                        stab: None,
                        dec,
                    };
                    def::insert_static_field(active, &df.dec.1, &df)?;
                }
                let do_promote_fields_to_public_vis =
                // if the package_name is ⛔, then "promote" everything to be public.
                    active.package().as_ref().map_or(false, |n| n.as_str() == "⛔");
                let fields = // promote.
                    if do_promote_fields_to_public_vis {
                        Delim{
                            vec:
                            init.fields.vec.iter().map(
                                |f|
                                crate::ast::NodeData(
                                    DecField{
                                        vis:Some(
                                            crate::ast::NodeData(crate::ast::Vis::Public(None),
                                                                 crate::ast::Source::ImportPrim).share()),
                                        .. f.0.clone()},
                                    f.1.clone()).share()).collect(),
                            .. init.fields.clone()
                        }
                    } else {
                        init.fields.clone()
                    };
                let v = def::module(
                    active,
                    path.clone(),
                    &init.id,
                    Source::CoreSetModule,
                    None,
                    None,
                    &fields,
                    None,
                )?;
                active.defs().leave_context(saved, &ctxid);
                *active.package() = importing_package;
                if let Some(top_path) = active.module_files().import_stack.pop_back() {
                    match active.module_files().import_stack.head() {
                        Some(active_module_path) => {
                            active.defs().active_path = Some(active_module_path.local_path.clone())
                        }
                        None => active.defs().active_path = None,
                    };
                    assert_eq!(top_path, path)
                } else {
                    unreachable!()
                };
                if let Value::Module(m) = &*v {
                    let mf = ModuleFile {
                        file_content: init.file_content.clone(),
                        context: m.context.clone(),
                        module: m.fields.clone(),
                    };
                    active
                        .module_files()
                        .map
                        .insert(path.clone(), ModuleFileState::Defined(mf.clone()));
                    mf
                } else {
                    unreachable!()
                }
            }
        };
        log::debug!("`import {}` Success.", path0);
        Ok(mf.def())
    }

    // temporary code quality, for now.
    // the idea is to give a human readable warning about each attribute that we
    // notice but do not yet handle correctly.
    // for now, that's every attribute.
    //
    // the first attribute that we want to implement, somehow, is "listing".
    // the idea is to create a separate file with just that dec field pretty-printed into it.
    // the name of the file will be based on the current module path
    //
    fn dec_field_kind_and_id(df: &DecField) -> (String, String) {
        match &df.dec.0 {
            Dec::Type(id, _, _) => ("type".to_string(), id.0.to_string()),
            Dec::Func(f) => {
                let f = f
                    .name
                    .clone()
                    .map(|id| id.0.id.0.to_string())
                    .unwrap_or("".to_string());
                ("func".to_string(), f)
            }
            _ => todo!(),
        }
    }

    pub fn insert_static_field<A: Active>(
        active: &mut A,
        source: &Source,
        df: &DecField,
    ) -> Result<(), Interruption> {
        if let Some(ref attrs) = df.attrs {
            if attrs.vec.len() > 0 {
                let (kind, id) = dec_field_kind_and_id(df);
                debug!("Attributes on {}-{} {:?}", kind, id, &df.attrs);
            }
            // to do -- introduce better semantic logic for attributes.
            if let Some(attr) = attrs.vec.iter().find(|attr| match &attr.0 {
                crate::ast::Attr::Id(id) => id.0.as_str() == "listing",
                crate::ast::Attr::Call(id, _) => id.0.as_str() == "listing",
                crate::ast::Attr::Field(_, _) => false,
            }) {
                let file = active
                    .module_files()
                    .import_stack
                    .back()
                    .map(|m| m.local_path.clone());
                let (kind, id) = dec_field_kind_and_id(df);
                info!(
                    "Listing: {}: {:?}\n{}",
                    file.clone().unwrap_or("(no file)".to_string()),
                    &attr.1,
                    format_pretty(&df.dec, 80)
                );
                let id_symbol = Symbol::Id(Id::new(id));
                let id_symbol = Symbol::Dot(
                    Shared::new(id_symbol),
                    Shared::new(Symbol::Id(Id::new(format!("fumola-{}-listing", kind)))),
                );
                let symbol = match file {
                    None => id_symbol,
                    Some(file) => {
                        let file_symbol = Shared::new(Symbol::Id(Id::new(file)));
                        Symbol::Dot(file_symbol, Shared::new(id_symbol))
                    }
                };
                active.output_files().insert(
                    symbol,
                    Text::new(format_pretty(&df.dec, 80)).append(&Text::new("\n".to_string())),
                );
            }
        };
        //println!("{:?} -- {:?} ", source, df);
        match &df.dec.0 {
            Dec::Attrs(_attrs, _dec) => {
                nyi!(line!())
            }
            Dec::LetModule(id, _, dfs) => {
                let v = module(
                    active,
                    ModulePath {
                        package_name: None,
                        local_path: format!("<anonymous@{}>", &df.dec.1),
                    },
                    &id.clone().map(|i| i.0.id_()),
                    df.dec.1.clone(),
                    df.vis.clone(),
                    df.stab.clone(),
                    dfs.dec_fields(),
                    None,
                )?;
                if let Some(id) = id {
                    if let Value::Module(m) = &*v {
                        active.defs().insert_field(
                            id.0.id_ref(),
                            source.clone(),
                            df.vis.clone(),
                            df.stab.clone(),
                            Def::Module(m.clone()),
                        )?;
                    };
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            Dec::Func(f) => {
                if let Some(name) = f.name.clone() {
                    let f = FunctionDef {
                        context: active.defs().active_ctx.clone(),
                        function: f.clone(),
                        rec_value: Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share(),
                    };
                    active.defs().insert_field(
                        name.0.id_ref(),
                        source.clone(),
                        df.vis.clone(),
                        df.stab.clone(),
                        Def::Func(f),
                    )?;
                    Ok(())
                } else {
                    nyi!(line!())
                }
            }
            Dec::Var(_p, _e) => Err(Interruption::ModuleNotStatic(
                df.dec.1.clone(),
                Some("var-decl".to_string()),
            )),
            Dec::Exp(e) => {
                if exp_is_static(&e.0) {
                    // ignore pure expression with no name.
                    Ok(())
                } else {
                    Err(Interruption::ModuleNotStatic(
                        df.dec.1.clone(),
                        Some(format!("non-static-exp({:?})", e)),
                    ))
                }
            }
            Dec::Let(p, e) => {
                if let Pat::Wild = p.0 {
                    if exp_is_static(&e.0) {
                        // ignore pure expression with no name.
                        Ok(())
                    } else {
                        Err(Interruption::ModuleNotStatic(
                            df.dec.1.clone(),
                            Some(format!("non-static-let({:?})", e)),
                        ))
                    }
                } else {
                    match crate::vm_match::get_pat_var(&p.0) {
                        Some(x) => {
                            let ctx_id = active.defs().active_ctx.clone();
                            if exp_is_static(&e.0) {
                                active.defs().insert_field(
                                    &x.0,
                                    source.clone(),
                                    df.vis.clone(),
                                    df.stab.clone(),
                                    Def::StaticValue(ctx_id, e.clone()),
                                )?;
                                Ok(())
                            } else {
                                Err(Interruption::ModuleNotStatic(
                                    source.clone(),
                                    Some(format!("non-static-let({:?})", e)),
                                ))
                            }
                        }
                        None => {
                            nyi!(line!())
                        }
                    }
                }
            }
            Dec::LetImport(pattern, _, path) => {
                let m = import(active, path)?;
                let fields = module_project(active.defs(), &m, &pattern.0)?;
                for (x, def) in fields {
                    active
                        .defs()
                        .insert_field(&x.0, x.1.clone(), None, None, def.clone())?;
                }
                Ok(())
            }
            Dec::Type(_id, _typ_binds, _typ) => {
                if let Some(ref attrs) = df.attrs {
                    if attrs.vec.len() > 0 {
                        // warn!("Ignoring type attributes: {:?}", df.attrs);
                        // warn!("{}", format_one_line(&df.to_motoko().unwrap()));
                        // return nyi!(line!());
                    }
                };
                Ok(())
            }
            Dec::LetActor(_i, _, _dfs) => {
                nyi!(line!())
            }
            Dec::LetObject(_i, _, _dfs) => {
                nyi!(line!())
            }
            Dec::Class(_class) => {
                // 20221209-1047 to do.
                // nyi!(line!())
                Ok(())
            }
        }
    }

    fn insert_owned_field<A: Active>(
        active: &mut A,
        def_owner: &ScheduleChoice,
        source: &Source,
        df: &DecField,
    ) -> Result<(), Interruption> {
        //println!("{:?} -- {:?} ", source, df);
        match &df.dec.0 {
            Dec::Func(f) => {
                if let Some(name) = f.name.clone() {
                    let f = FunctionDef {
                        context: active.defs().active_ctx.clone(),
                        function: f.clone(),
                        rec_value: Value::Function(ClosedFunction(Closed {
                            ctx: active.defs().active_ctx.clone(),
                            env: active.env().fast_clone(),
                            content: f.clone(),
                        }))
                        .share(),
                    };
                    active.defs().insert_field(
                        name.0.id_ref(),
                        source.clone(),
                        df.vis.clone(),
                        df.stab.clone(),
                        Def::Func(f),
                    )?;
                    Ok(())
                } else {
                    nyi!(line!())
                }
            }
            Dec::Var(p, e) => {
                let v = match &e.0 {
                    Exp::Literal(l) => Value::from_literal(l)?,
                    _ => return Err(Interruption::NonLiteralInit(e.1.clone())),
                };
                let mut pat = &p.0;
                if let Pat::AnnotPat(ref p, _) = pat {
                    // Temporary
                    pat = &p.0;
                }
                match pat {
                    Pat::Var(ref x) => {
                        let var_def = Def::Var(VarDef {
                            owner: def_owner.clone(),
                            name: x.0.clone(),
                            init: v.share(),
                        });
                        active.defs().insert_field(
                            &x.0,
                            source.clone(),
                            df.vis.clone(),
                            df.stab.clone(),
                            var_def,
                        )?;
                        //Ok(Value::Pointer(Pointer::NamedPointer(x.0.clone())))
                        Ok(())
                    }
                    _ => nyi!(line!()),
                }
            }
            _ => nyi!(line!()),
        }
    }

    pub fn actor<A: Active>(
        active: &mut A,
        path: String,
        id: &ActorId,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
    ) -> Result<Value_, Interruption> {
        let (parent, fields) = active.defs().enter_context(false);
        for df in dfs.vec.iter() {
            insert_owned_field(active, &ScheduleChoice::Actor(id.clone()), &df.1, &df.0)?;
        }
        active.defs().leave_context(parent, &fields);
        let context = active.defs().active_context();
        let actor = crate::vm_types::def::Actor { context, fields };
        match id {
            ActorId::Alias(_x) => {
                /* 20221103
                active
                    .defs()
                    .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
                */
            }
            ActorId::Local(x) => {
                active
                    .defs()
                    .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            }
        }
        active.create(path, id.clone(), actor)
    }

    pub fn actor_upgrade<A: Active>(
        active: &mut A,
        path: String,
        id: &ActorId,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
        old_def: &ActorDef,
    ) -> Result<Value_, Interruption> {
        let (saved, fields, old_ctx) = active.defs().reenter_context(&old_def.fields);
        for df in dfs.vec.iter() {
            insert_owned_field(active, &ScheduleChoice::Actor(id.clone()), &df.1, &df.0)?;
        }
        active.defs().releave_context(saved, &fields, &old_ctx);
        let context = active.defs().active_context();
        let actor = crate::vm_types::def::Actor { context, fields };
        match id {
            ActorId::Alias(_x) => {
                /*
                active
                    .defs()
                    .reinsert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
                */
            }
            ActorId::Local(x) => {
                active
                    .defs()
                    .reinsert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            }
        }
        active.upgrade(path, id.clone(), actor)
    }

    pub fn module<A: Active>(
        active: &mut A,
        path: ModulePath,
        id: &Option<Id_>,
        source: Source,
        vis: Option<Vis_>,
        stab: Option<Stab_>,
        dfs: &DecFields,
        // Some(_) means the module is being upgraded.
        ctx_id: Option<CtxId>,
    ) -> Result<Value_, Interruption> {
        if let Some(ctx_id) = ctx_id {
            let (saved, fields, old_ctx) = active.defs().reenter_context(&ctx_id);
            for df in dfs.vec.iter() {
                insert_static_field(active, &df.1, &df.0)?;
            }
            active.defs().releave_context(saved, &fields, &old_ctx);
            let context = active.defs().active_context();
            let module = crate::vm_types::def::Module { context, fields };
            match id {
                None => (),
                Some(x) => active.defs().reinsert_field(
                    &x.0,
                    source,
                    vis,
                    stab,
                    Def::Module(module.clone()),
                )?,
            }
            active.upgrade_module(path, id.as_ref().map(|x| x.0.clone()), module)
        } else {
            let (parent, fields) = active.defs().enter_context(false);
            for df in dfs.vec.iter() {
                insert_static_field(active, &df.1, &df.0)?;
            }
            active.defs().leave_context(parent, &fields);
            let context = active.defs().active_context();
            let module = crate::vm_types::def::Module { context, fields };
            /*
                    active
                        .defs()
                        .insert_field(&x, source, vis, stab, Def::Actor(actor.clone()))?
            */
            active.create_module(path, id.as_ref().map(|x| x.0.clone()), module)
        }
    }
}

fn exp_field_is_static(e: &ExpField) -> bool {
    if let Some(e) = &e.exp {
        exp_is_static(&e.0)
    } else {
        true
    }
}

fn object_is_static(d: &crate::ast::Delim<ExpField_>) -> bool {
    for ef in d.vec.iter() {
        if !exp_field_is_static(&ef.0) {
            return false;
        }
    }
    true
}

fn delim_is_static(d: &crate::ast::Delim<Exp_>) -> bool {
    for e in d.vec.iter() {
        if !exp_is_static(&e.0) {
            return false;
        }
    }
    true
}

// Conservative check if an expression is "static enough".
// No allocation, looping is permitted in a static expression.
// (So no Mut::Var fields, and no objects, etc.)
fn exp_is_static(e: &Exp) -> bool {
    match e {
        Exp::ActorUrl(_) => true,
        Exp::Object((_, efs)) => {
            if let Some(efs) = efs {
                object_is_static(efs)
            } else {
                true
            }
        }
        Exp::QuotedAst(_a) => true,
        Exp::Thunk(_) => true,
        Exp::Literal(_) => true,
        Exp::Function(_) => true,
        Exp::Block(decs) => {
            if decs.vec.len() > 1 {
                return false;
            };
            decs.vec.iter().all(|d| dec_is_static(&d.0))
        }
        Exp::Var(_) => true,  // variables reference _values_ in the env.
        Exp::Prim(_) => true, // primitives are constant/built-in.
        Exp::Un(_, e1) => exp_is_static(&e1.0),
        Exp::Bin(e1, _, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Rel(e1, _, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Opt(e) => exp_is_static(&e.0),
        Exp::Variant(_, None) => true,
        Exp::Variant(_, Some(e)) => exp_is_static(&e.0),
        Exp::Array(Mut::Const, d) => delim_is_static(d),
        Exp::Tuple(d) => delim_is_static(d),
        Exp::Not(e) => exp_is_static(&e.0),
        Exp::And(e1, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::Or(e1, e2) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::If(e1, e2, None) => exp_is_static(&e1.0) & exp_is_static(&e2.0),
        Exp::If(e1, e2, Some(e3)) => {
            exp_is_static(&e1.0) & exp_is_static(&e2.0) & exp_is_static(&e3.0)
        }
        // Switch -- to do
        Exp::Ignore(e) => exp_is_static(&e.0),
        Exp::Annot(_, e, _) => exp_is_static(&e.0),
        Exp::Paren(e) => exp_is_static(&e.0),
        Exp::Dot(e1, _e2) => exp_is_static(&e1.0) & exp_is_static(&e1.0),
        _ => {
            log::warn!("Safety check failed: exp_is_static({:?})", e);
            false
        }
    }
}

fn dec_is_static(d: &Dec) -> bool {
    match d {
        Dec::Exp(e) => exp_is_static(&e.0),
        Dec::Func(_) => true,
        _ => todo!("dec_is_static({:?})", d),
    }
}

pub fn def_as_value(defs: &Defs, i: &Id, def: &Def) -> Result<Value_, Interruption> {
    match def {
        Def::Actor(a) => Ok(Value::Actor(crate::value::Actor {
            def: Some(a.clone()),
            id: ActorId::Local(i.clone()),
        })
        .share()),
        Def::Func(f) => Ok(f.rec_value.fast_clone()),
        Def::StaticValue(ctx_id, e) => {
            let mut c = Core::empty();
            c.defs = defs.clone();
            c.defs.active_ctx = ctx_id.clone();
            c.agent.active.cont = Cont::Exp_(e.fast_clone(), Vector::new());
            let v = c.run(&Limits::none())?;
            Ok(v)
        }
        Def::Var(v) => Ok(crate::value::Value::Pointer(Pointer {
            owner: v.owner.clone(),
            local: LocalPointer::Named(NamedPointer(v.name.clone())),
        })
        .share()),
        Def::Module(m) => Ok(Value::Module(m.clone()).share()),
    }
}

pub fn resolve_def(
    defs: &Defs,
    ctx_id: &CtxId,
    is_public_projection: bool,
    x: &Id,
) -> Result<FieldDef, Interruption> {
    let ctx = defs.map.get(&ctx_id).unwrap();
    match ctx.fields.get(x) {
        Some(d) => {
            let f_is_public = match &d.vis {
                Some(x) => x.0.is_public(),
                None => false,
            };
            if is_public_projection && !f_is_public {
                return Err(Interruption::ModuleFieldNotPublic(x.clone()));
            };
            Ok(d.clone())
        }
        None => {
            if is_public_projection {
                Err(Interruption::UnboundIdentifer(x.clone()))
            } else {
                match &ctx.parent {
                    Some(p) => resolve_def(defs, p, false, x),
                    None => Err(Interruption::UnboundIdentifer(x.clone())),
                }
            }
        }
    }
}

impl crate::vm_types::def::Field {
    pub fn source(&self) -> Source {
        self.def.source()
    }
}
