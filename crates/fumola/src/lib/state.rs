use crate::check;
use crate::Error;
use fumola_semantics::value::ActorId;
use fumola_semantics::value::Value_;
use fumola_semantics::Interruption;
use fumola_semantics::{
    vm_def::def,
    vm_types::{Active, Core, Limits, ModuleFileState, ModulePath},
};
use fumola_syntax::ast::Id;
use fumola_syntax::ast::{DecField, Source};

pub struct State {
    pub semantic_state: Core,
}

impl State {
    pub fn empty() -> Self {
        State {
            semantic_state: Core::empty(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<Value_, crate::Error> {
        let prog = check::parse(input)?;
        Ok(self.semantic_state.eval_prog(prog)?)
    }

    /// Call an actor method.
    pub fn call(
        &mut self,
        actor: &ActorId,
        method: &Id,
        arg: Value_,
    ) -> Result<Value_, crate::Error> {
        self.semantic_state
            .call(actor, method, arg, &Limits::none())
            .map_err(|x| x.into())
    }

    /// Load `base` library into an existing Core.
    pub fn load_base(&mut self) -> Result<(), crate::Error> {
        use crate::package::{get_base_library, get_prim_library};
        let prim = get_prim_library();
        for (path, file) in prim.files.into_iter() {
            // remove '.mo' from suffix of the filename to produce the path
            let path = format!("{}", &path[0..path.len() - 3]);
            self.set_module(Some("⛔".to_string()), path.clone(), &file.content)?;
        }
        let base = get_base_library();
        for (path, file) in base.files.into_iter() {
            // remove '.mo' from suffix of the filename to produce the path
            let path = format!("{}", &path[0..path.len() - 3]);
            self.set_module(Some("base".to_string()), path.clone(), &file.content)?
        }
        Ok(())
    }

    /// Set the actor `id` to the given `definition`, regardless of whether `id` is defined already or not.
    /// If not defined, this is the same as `create_actor`.
    /// Otherwise, it is the same as `update_actor`.
    pub fn set_actor(&mut self, path: String, id: ActorId, def: &str) -> Result<(), Error> {
        if self.semantic_state.actors.map.get(&id).is_none() {
            self.create_actor(path, id, def)
        } else {
            self.upgrade_actor(path, id, def)
        }
    }

    /// Create a new actor with the given (unused) `id`, and the definition `def`.
    pub fn create_actor(
        &mut self,
        path: String,
        id: ActorId,
        def: &str,
    ) -> Result<(), crate::Error> {
        if let Some(_) = self.semantic_state.actors.map.get(&id) {
            return Err(Interruption::AmbiguousActorId(id).into());
        };
        let (decs, _id, dfs) = check::assert_actor_def(path.clone(), def)?;
        let (saved, new_root) = self.semantic_state.defs().enter_context(true);
        for dec in decs.iter() {
            let dec = dec.clone();
            let df = fumola_syntax::ast::DecField {
                attrs: None,
                vis: None,
                stab: None,
                dec,
            };
            def::insert_static_field(&mut self.semantic_state, &df.dec.1, &df)?;
        }
        def::actor(
            &mut self.semantic_state,
            path,
            &id,
            Source::CoreCreateActor,
            None,
            None,
            &dfs,
        )?;
        self.semantic_state.defs().leave_context(saved, &new_root);
        Ok(())
    }

    /// Upgrade an existing actor with the given `id`, with new definition `def`.
    pub fn upgrade_actor(&mut self, path: String, id: ActorId, def: &str) -> Result<(), Error> {
        let old_def = if let Some(old) = self.semantic_state.actors.map.get(&id) {
            old.def.clone()
        } else {
            return Err(Interruption::ActorIdNotFound(id).into());
        };
        let (decs, _id, dfs) = check::assert_actor_def(path.clone(), def)?;
        let (saved, ctxid, old_ctx) = self.semantic_state.defs().reenter_context(&old_def.context);
        for dec in decs.iter() {
            let dec = dec.clone();
            let df = fumola_syntax::ast::DecField {
                attrs: None,
                vis: None,
                stab: None,
                dec,
            };
            def::insert_static_field(&mut self.semantic_state, &df.dec.1, &df)?;
        }
        def::actor_upgrade(
            &mut self.semantic_state,
            path,
            &id,
            Source::CoreUpgradeActor,
            None,
            None,
            &dfs,
            &old_def,
        )?;
        self.semantic_state
            .defs()
            .releave_context(saved, &ctxid, &old_ctx);
        Ok(())
    }

    /// Evaluate a new program fragment, NOT assuming agent is idle.
    /// generally, self.agent.active.stack is non-empty, and we are trying to return something to it that got stuck earlier.
    pub fn resume(&mut self, new_prog_frag: &str) -> Result<Value_, Error> {
        let local_path = "<anonymous>".to_string();
        let package_name = None;
        let p = crate::check::parse(new_prog_frag).map_err(|code| {
            Error::SyntaxError(crate::SyntaxError {
                code,
                local_path,
                package_name,
            })
        })?;
        self.semantic_state.agent.active.cont = fumola_semantics::vm_types::Cont::Decs(p.vec);
        let v = self.semantic_state.run(&Limits::none())?;
        Ok(v)
    }

    /// Set the path's file content (initially), or re-set it, when it changes.
    ///
    /// Optionally, the file is part of a named package, and will be distinct from paths from other packages.
    ///
    /// The content must be a module.  For actors, see `set_actor` instead.
    pub fn set_module(
        &mut self,
        package_name: Option<String>,
        mut local_path: String,
        file_content: &str,
    ) -> Result<(), crate::Error> {
        let local_path_ends_with_fumola = local_path.ends_with(".fumola");
        let local_path_ends_with_mo = !local_path_ends_with_fumola && local_path.ends_with(".mo");
        if local_path_ends_with_fumola {
            local_path = format!("{}", &local_path[0..local_path.len() - 7]);
        } else if local_path_ends_with_mo {
            local_path = format!("{}", &local_path[0..local_path.len() - 3]);
        }
        let path = ModulePath {
            package_name,
            local_path,
        };
        let init = crate::check::assert_module_def(path.clone(), file_content)?;
        let old = self
            .semantic_state
            .module_files
            .map
            .get(&path)
            .map(|x| x.clone());
        if let Some(ModuleFileState::Defined(old)) = old {
            let (saved, ctxid, old_ctx) = self.semantic_state.defs().reenter_context(&old.context);
            for dec in init.outer_decs.iter() {
                let dec = dec.clone();
                let df = DecField {
                    attrs: None,
                    vis: None,
                    stab: None,
                    dec,
                };
                def::insert_static_field(&mut self.semantic_state, &df.dec.1, &df)?;
            }
            fumola_semantics::vm_def::def::module(
                &mut self.semantic_state,
                path,
                &init.id,
                Source::CoreSetModule,
                None,
                None,
                &init.fields,
                Some(old.module.clone()),
            )?;
            self.semantic_state
                .defs()
                .releave_context(saved, &ctxid, &old_ctx);
        } else {
            self.semantic_state
                .module_files
                .map
                .insert(path.clone(), ModuleFileState::Init(init));
        };
        Ok(())
    }
}
