use crate::adapton::AdaptonState;
use crate::value::{ActorId, ActorMethod, Value, Value_};
use crate::vm_types::Env;
use crate::vm_types::Stack;
use crate::vm_types::def::{CtxId, Function};
use crate::vm_types::{self, Actor};
use crate::vm_types::{
    Activation, Active, Actors, Agent, Cont, Core, Counts, Interruption, Limits, ModuleFiles,
    ModulePath, Pointer, Response, ScheduleChoice, Step,
    def::{Actor as ActorDef, Def, Defs, Field as FieldDef, Module as ModuleDef},
    stack::{Frame, FrameCont},
};
use crate::vm_types::{ActiveBorrow, OutputFiles};
use crate::vm_types::{DebugPrintLine, TestSuite};
use crate::vm_types::{EvalInitError, Store};
use crate::vm_types::{LocalPointer, NamedPointer};
use crate::{nyi, type_mismatch};
use fumola_syntax::ast::PrimType;
use fumola_syntax::ast::{Exp_, Id, Inst, Prog, Source, ToId};
use fumola_syntax::shared::{FastClone, Share};
use im_rc::{HashMap, Vector};
use std::vec::Vec;

fn agent_init(prog: Prog) -> Agent {
    let mut a = Agent {
        store: Store::new(ScheduleChoice::Agent),
        //debug_print_out: Vector::new(),
        adapton_state: crate::adapton::state::State::new(crate::adapton::Strategy::Graphical),
        counts: Counts::default(),
        active: Activation::new(),
    };
    a.active.cont = Cont::Decs(prog.vec);
    a
}

impl Active for Core {
    fn ctx_id<'a>(&'a mut self) -> &'a mut CtxId {
        &mut self.defs.active_ctx
    }
    fn defs<'a>(&'a mut self) -> &'a mut Defs {
        &mut self.defs
    }
    fn module_files<'a>(&'a mut self) -> &'a mut ModuleFiles {
        &mut self.module_files
    }
    fn output_files<'a>(&'a mut self) -> &'a mut OutputFiles {
        &mut self.output_files
    }
    fn test_suite<'a>(&'a mut self) -> &'a mut TestSuite {
        &mut self.test_suite
    }
    //fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice {
    //&self.schedule_choice
    //}
    fn cont<'a>(&'a mut self) -> &'a mut Cont {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont
            }
        }
    }
    fn package<'a>(&'a mut self) -> &'a mut Option<String> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.package,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .package
            }
        }
    }
    fn cont_source<'a>(&'a mut self) -> &'a mut Source {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont_source,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont_source
            }
        }
    }
    fn cont_prim_type<'a>(&'a mut self) -> &'a mut Option<PrimType> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.cont_prim_type,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .cont_prim_type
            }
        }
    }
    fn env<'a>(&'a mut self) -> &'a mut Env {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.env,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .env
            }
        }
    }
    fn stack<'a>(&'a mut self) -> &'a mut Stack {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.active.stack,
            Actor(n) => {
                &mut self
                    .actors
                    .map
                    .get_mut(n)
                    .unwrap()
                    .active
                    .as_mut()
                    .unwrap()
                    .stack
            }
        }
    }
    fn store<'a>(&'a mut self) -> &'a mut Store {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.store,
            Actor(n) => &mut self.actors.map.get_mut(n).unwrap().store,
        }
    }
    fn debug_print_out<'a>(&'a mut self) -> &'a mut Vector<DebugPrintLine> {
        &mut self.debug_print_out
    }
    fn counts<'a>(&'a mut self) -> &'a mut Counts {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.counts,
            Actor(n) => &mut self.actors.map.get_mut(n).unwrap().counts,
        }
    }

    fn adapton<'a>(&'a mut self) -> &'a mut crate::adapton::state::State {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &mut self.agent.adapton_state,
            Actor(n) => &mut self.actors.map.get_mut(n).unwrap().adapton_state,
        }
    }

    fn create(
        &mut self,
        path: String,
        name: ActorId,
        def: ActorDef,
    ) -> Result<Value_, Interruption> {
        let v = Value::Actor(crate::value::Actor {
            def: Some(def.clone()),
            id: name.clone(),
        });
        //let def = self.defs().map.get(&CtxId(0)).unwrap().fields.get(name).unwrap().def.clone();
        let adapton_state = crate::adapton::state::State::new(crate::adapton::Strategy::Graphical);
        let mut store = Store::new(ScheduleChoice::Actor(name.clone()));
        let mut env = self.env().clone();
        let ctx = self.def_ctx(&def.fields, line!())?.clone();
        for (i, field) in ctx.fields.iter() {
            match &field.def {
                Def::Var(v) => {
                    store.alloc_named(i.clone(), v.init.fast_clone())?;
                    let owner = ScheduleChoice::Actor(name.clone());
                    env.insert(
                        i.clone(),
                        Value::Pointer(Pointer {
                            owner,
                            local: LocalPointer::Named(NamedPointer(i.clone())),
                        })
                        .share(),
                    );
                }
                Def::Func(f) => {
                    env.insert(i.clone(), f.rec_value.fast_clone());
                }
                _ => return nyi!(line!()),
            }
        }
        let a = Actor {
            path,
            def,
            env,
            store,
            adapton_state,
            counts: Counts::default(),
            active: None,
            awaiting: HashMap::new(),
        };
        let a0 = self.actors.map.insert(name, a);
        if let Some(_a0) = a0 {
            return nyi!(line!(), "creating an actor over one that already exists");
        };
        Ok(v.share())
    }

    fn upgrade(
        &mut self,
        path: String,
        name: ActorId,
        def: ActorDef,
    ) -> Result<Value_, Interruption> {
        let v = Value::Actor(crate::value::Actor {
            def: Some(def.clone()),
            id: name.clone(),
        });
        let mut env = HashMap::new();
        let (mut store, counts) = {
            let actor = self
                .actors
                .map
                .get(&name)
                .ok_or_else(|| Interruption::ActorIdNotFound(name.clone()))?;
            (actor.store.clone(), actor.counts.clone())
        };
        let adapton_state = crate::adapton::state::State::new(crate::adapton::Strategy::Graphical);
        let ctx = self.def_ctx(&def.fields, line!())?.clone();
        for (i, field) in ctx.fields.iter() {
            match &field.def {
                Def::Var(v) => {
                    match store.get(&LocalPointer::Named(NamedPointer(i.clone()))) {
                        None => {
                            let p = store.alloc_named(i.clone(), v.init.fast_clone())?;
                            let pv = Value::Pointer(p).share();
                            env.insert(i.clone(), pv);
                        }
                        Some(_) => {
                            let p = Pointer {
                                owner: ScheduleChoice::Actor(name.clone()),
                                local: LocalPointer::Named(NamedPointer(i.clone())),
                            };
                            let pv = Value::Pointer(p).share();
                            // keep store's current value.
                            // (even if not stable.)
                            env.insert(i.clone(), pv);
                        }
                    }
                }
                Def::Func(..) => {
                    // to do
                }
                other => {
                    return nyi!(
                        line!(),
                        "upgrading an actor whose field {} is a {}",
                        i.as_str(),
                        other.kind_name()
                    );
                }
            }
        }
        let a = Actor {
            path,
            def,
            env,
            store,
            adapton_state,
            counts,
            active: None,
            awaiting: HashMap::new(),
        };
        self.actors.map.insert(name, a);
        Ok(v.share())
    }

    fn create_module(
        &mut self,
        _path: ModulePath,
        _id: Option<Id>,
        module: ModuleDef,
    ) -> Result<Value_, Interruption> {
        Ok(crate::value::Value::Module(module).share())
    }

    fn upgrade_module(
        &mut self,
        _path: ModulePath,
        _id: Option<Id>,
        module: ModuleDef,
    ) -> Result<Value_, Interruption> {
        Ok(crate::value::Value::Module(module).share())
    }
}

impl ActiveBorrow for Core {
    fn ctx_id<'a>(&'a self) -> &'a CtxId {
        &self.defs.active_ctx
    }
    fn defs<'a>(&'a self) -> &'a Defs {
        &self.defs
    }
    fn schedule_choice<'a>(&'a self) -> &'a ScheduleChoice {
        &self.schedule_choice
    }
    fn cont<'a>(&'a self) -> &'a Cont {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont,
            Actor(n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont
            }
        }
    }
    fn cont_source<'a>(&'a self) -> &'a Source {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont_source,
            Actor(n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont_source
            }
        }
    }
    fn cont_prim_type<'a>(&'a self) -> &'a Option<PrimType> {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.cont_prim_type,
            Actor(n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .cont_prim_type
            }
        }
    }
    fn env<'a>(&'a self) -> &'a Env {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.env,
            Actor(n) => &self.actors.map.get(n).unwrap().active.as_ref().unwrap().env,
        }
    }
    fn stack<'a>(&'a self) -> &'a Stack {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.active.stack,
            Actor(n) => {
                &self
                    .actors
                    .map
                    .get(n)
                    .unwrap()
                    .active
                    .as_ref()
                    .unwrap()
                    .stack
            }
        }
    }
    fn store<'a>(&'a self) -> &'a Store {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.store,
            Actor(n) => &self.actors.map.get(n).unwrap().store,
        }
    }
    fn debug_print_out<'a>(&'a self) -> &'a Vector<DebugPrintLine> {
        &self.debug_print_out
    }
    fn counts<'a>(&'a self) -> &'a Counts {
        use ScheduleChoice::*;
        match &self.schedule_choice {
            Agent => &self.agent.counts,
            Actor(n) => &self.actors.map.get(n).unwrap().counts,
        }
    }
}

impl Limits {
    pub fn default() -> Self {
        Self::none()
    }

    /// No limits.
    pub fn none() -> Limits {
        Limits {
            breakpoints: vec![],
            step: None,
            redex: None,
            send: None,
        }
    }
    /// Set step limit.
    pub fn step(mut self, s: usize) -> Self {
        self.step = Some(s);
        self
    }
    /// Set redex limit.
    pub fn redex(mut self, s: usize) -> Self {
        self.redex = Some(s);
        self
    }
}

impl Core {
    /// New VM for a given program.
    pub fn new(prog: Prog) -> Self {
        Core {
            defs: Defs::new(),
            schedule_choice: ScheduleChoice::Agent,
            agent: agent_init(prog),
            actors: Actors {
                map: HashMap::new(),
            },
            module_files: ModuleFiles {
                map: HashMap::new(),
                import_stack: Vector::new(),
            },
            test_suite: HashMap::new(),
            next_resp_id: 0,
            debug_print_out: Vector::new(),
            output_files: HashMap::new(),
        }
    }

    /// New VM without any program.
    pub fn empty() -> Self {
        let mut c = Self::new(fumola_syntax::ast::Delim::new());
        c.run(&Limits::none()).expect("empty");
        c
    }

    /// Call an actor method.
    pub fn call_method(
        &mut self,
        actor: &ActorId,
        method: &Id,
        arg: Value_,
        limits: &Limits,
    ) -> Result<Value_, Interruption> {
        self.assert_idle_agent()?;
        let fn_v = Value::ActorMethod(ActorMethod {
            actor: actor.clone(),
            method: method.clone(),
        })
        .share();
        let context = self.defs().active_ctx.clone();

        self.stack().push_front(Frame {
            context,
            env: HashMap::new(),
            cont: FrameCont::Call2(fn_v, None),
            source: Source::CoreCall,
            cont_prim_type: None,
        });
        *self.cont() = Cont::Value_(arg);
        *self.cont_source() = Source::CoreCall;
        self.run(limits)
    }

    /// Call an actor method.
    pub fn call_function_def(
        &mut self,
        function: Function,
        arg: Value_,
    ) -> Result<Value_, Interruption> {
        self.assert_idle_agent()?;

        let _ = crate::vm_step::call_function_def(self, HashMap::new(), &function, None, arg)?;
        self.run(&Limits::none())
    }

    /// Attempt a single-step of VM, under some limits.
    /// The definition context with this id, which the caller expects to exist.
    fn def_ctx(
        &mut self,
        id: &CtxId,
        line: u32,
    ) -> Result<&crate::vm_types::def::Ctx, Interruption> {
        match self.defs.map.get(id) {
            Some(ctx) => Ok(ctx),
            None => Err(crate::impossible_!(
                line,
                "context {:?} is named by a definition but not held by the def table",
                id
            )),
        }
    }

    /// That the agent named by `schedule_choice` is one this core can step.
    ///
    /// The `Active` accessors -- `cont`, `env`, `store`, `counts`, `adapton`
    /// -- each hand back a `&mut` into the scheduled agent's own state, and a
    /// borrow has no room to carry an `Interruption` out. So the question they
    /// cannot ask is asked here instead, once, before any of them runs: every
    /// path that steps a core comes through `step`, and a core that fails this
    /// check stops with an error rather than inside an accessor.
    fn check_schedule_choice(&self) -> Result<(), Interruption> {
        let ScheduleChoice::Actor(id) = &self.schedule_choice else {
            return Ok(());
        };
        match self.actors.map.get(id) {
            None => Err(Interruption::ActorIdNotFound(id.clone())),
            Some(actor) if actor.active.is_none() => Err(crate::impossible_!(
                line!(),
                "actor {:?} is scheduled but has no activation to step",
                id
            )),
            Some(_) => Ok(()),
        }
    }

    pub fn step(&mut self, limits: &Limits) -> Result<Step, Interruption> {
        self.check_schedule_choice()?;
        match crate::vm_step::active_step(self, limits) {
            Ok(Step {}) => Ok(Step {}),
            Err(Interruption::Send(am, inst, v)) => self.send(limits, am, inst, v),
            Err(Interruption::Response(r)) => self.response(limits, r),
            Err(other_interruption) => return Err(other_interruption),
        }
    }

    fn get_public_actor_field(&self, a: &ActorId, m: &Id) -> Result<FieldDef, Interruption> {
        let actor = match self.actors.map.get(a) {
            Some(a) => a,
            None => return Err(Interruption::ActorIdNotFound(a.clone())),
        };
        let f = match actor.def.fields.get_field(self, &m) {
            None => return Err(Interruption::ActorFieldNotFound(a.clone(), m.clone())),
            Some(f) => f,
        };
        let f_is_public = match &f.vis {
            Some(x) => x.0.is_public(),
            None => false,
        };
        if !f_is_public {
            return Err(Interruption::ActorFieldNotPublic(a.clone(), m.clone()));
        };
        Ok(f.clone())
    }

    fn send(
        &mut self,
        _limits: &Limits,
        am: ActorMethod,
        inst: Option<Inst>,
        v: Value_,
    ) -> Result<Step, Interruption> {
        let context = self.defs().active_ctx.clone();
        let resp_target = self.schedule_choice.clone();
        self.schedule_choice = ScheduleChoice::Actor(am.actor.clone());
        let actor = self
            .actors
            .map
            .get(&am.actor)
            .ok_or_else(|| Interruption::ActorIdNotFound(am.actor.clone()))?;
        let actor_env = actor.env.fast_clone();
        let f = {
            let f = self.get_public_actor_field(&am.actor, &am.method)?;
            match &f.def {
                Def::Func(f) => f.clone(),
                _ => type_mismatch!(file!(), line!()),
            }
        };
        if actor.active.is_some() {
            // Calling into an actor that is part-way through a call of its own
            // would overwrite the activation it is still using.
            return crate::impossible!(
                line!(),
                "actor {:?} is already running, and cannot take the call to {}",
                am.actor,
                am.method.as_str()
            );
        }
        let mut activation = Activation::new();
        activation.stack.push_front(Frame {
            context,
            source: Source::Evaluation,
            cont_prim_type: None,
            env: actor.env.fast_clone(),
            cont: FrameCont::Respond(resp_target),
        });
        let actor = self
            .actors
            .map
            .get_mut(&am.actor)
            .ok_or_else(|| Interruption::ActorIdNotFound(am.actor.clone()))?;
        actor.active = Some(activation);
        crate::vm_step::call_function_def(self, actor_env, &f, inst, v)
    }

    fn response(&mut self, _limits: &Limits, r: Response) -> Result<Step, Interruption> {
        match self.schedule_choice {
            ScheduleChoice::Actor(ref i) => {
                let actor = self
                    .actors
                    .map
                    .get_mut(i)
                    .ok_or_else(|| Interruption::ActorIdNotFound(i.clone()))?;
                actor.active = None;
            }
            // Only an actor responds; the agent has nobody to respond to.
            ScheduleChoice::Agent => {
                return crate::impossible!(line!(), "the agent produced a response");
            }
        };
        self.schedule_choice = r.target;
        *self.cont() = Cont::Value_(r.value);
        Ok(Step {})
    }

    /// Run multiple steps of VM, with given limits.
    /// `Ok(value)` means that the Agent is idle.
    pub fn run(&mut self, limits: &Limits) -> Result<Value_, Interruption> {
        loop {
            match self.step(limits) {
                Ok(Step {}) => {}
                Err(Interruption::Done(v)) => return Ok(v),
                Err(i) => {
                    if Self::ends_the_computation(&i) {
                        self.unwind_scratch();
                    }
                    return Err(i);
                }
            }
        }
    }

    /// Whether an interruption is the end of this computation, or a pause in
    /// it that the caller is expected to resume from.
    fn ends_the_computation(i: &Interruption) -> bool {
        !matches!(
            i,
            Interruption::Done(_)
                | Interruption::Send(..)
                | Interruption::Response(_)
                | Interruption::Breakpoint(_)
                | Interruption::Limit(_)
        )
    }

    /// Put the adapton state back if a scratch was in progress when this
    /// failed.
    ///
    /// Nothing unwinds the stack on an interruption -- it is left standing
    /// deliberately, so an error report can read it -- which means a scratch
    /// frame's completion arm never runs. Without this, a branch that failed
    /// half way through left its writes in the session: measured, a scratch
    /// that wrote ``kept := 999`` and then failed an assertion left the
    /// session reading `?999` where it had held 41.
    ///
    /// The outermost scratch is the one to restore from. Its save predates
    /// every nested one, so putting it back undoes them all at once. Frames
    /// are pushed at the front, so the outermost is the last one found.
    fn unwind_scratch(&mut self) {
        // Only where there is a stack to read. An actor that is not running
        // has no active part, and `stack()` unwraps it -- so an interruption
        // raised while such an actor is the scheduled one would turn into a
        // panic here rather than the error it is. Two actor tests found this.
        let reachable = match &self.schedule_choice {
            ScheduleChoice::Agent => true,
            ScheduleChoice::Actor(n) => self
                .actors
                .map
                .get(n)
                .map_or(false, |a| a.active.is_some()),
        };
        if !reachable {
            return;
        }
        let saved = self.stack().iter().rev().find_map(|f| match &f.cont {
            vm_types::stack::FrameCont::Scratch(s) => Some((**s).clone()),
            _ => None,
        });
        if let Some(saved) = saved {
            self.adapton().restore(saved);
        }
    }

    pub fn agent_stack(&self) -> Result<vm_types::Stack, EvalInitError> {
        if self.schedule_choice != ScheduleChoice::Agent {
            return Err(EvalInitError::AgentNotScheduled);
        }
        Ok(self.agent.active.stack.clone())
    }

    pub fn agent_cont_source(&self) -> Result<fumola_syntax::ast::Source, EvalInitError> {
        if self.schedule_choice != ScheduleChoice::Agent {
            return Err(EvalInitError::AgentNotScheduled);
        }
        Ok(self.agent.active.cont_source.clone())
    }

    /// Assert that the Agent is idle.
    pub fn assert_idle_agent(&self) -> Result<(), EvalInitError> {
        if self.schedule_choice != ScheduleChoice::Agent {
            return Err(EvalInitError::AgentNotScheduled);
        }
        if !self.agent.active.stack.is_empty() {
            return Err(EvalInitError::NonEmptyStack);
        }
        match self.agent.active.cont {
            Cont::Value_(_) => {}
            _ => return Err(EvalInitError::NonValueCont),
        };
        Ok(())
    }

    /// For running snippets of code as if they were within a package.
    /// (They import that package's modules as if they are all local).
    pub fn set_ambient_package_name(
        &mut self,
        package: Option<String>,
    ) -> Result<(), Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.package = package;
        Ok(())
    }

    pub fn clear_cont(&mut self) {
        *self.cont() = Cont::Value_(Value::Unit.share());

        *self.stack() = Vector::new()
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    ///
    /// The block may refer to variables
    /// bound as arguments, and then forgotten after evaluation.
    pub fn eval_open_block(
        &mut self,
        value_bindings: Vec<(&str, impl Into<Value_>)>,
        prog: Prog,
    ) -> Result<Value_, Interruption> {
        let source = self.agent.active.cont_source.clone(); // to do -- use prog source
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        crate::vm_step::exp_conts_(
            self,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(prog.vec),
            source,
        )?;
        for (x, v) in value_bindings.into_iter() {
            let _ = self.agent.active.env.insert(x.to_id(), v.into());
        }
        self.run(&Limits::none())
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    pub fn eval_prog(&mut self, prog: Prog) -> Result<Value_, Interruption> {
        self.eval_prog_limited(prog, &Limits::none())
    }

    /// Evaluate a program, stopping where `limits` say to.
    ///
    ///    `Err(Interruption::Limit(_))` from this is a pause and not a
    /// failure. `run` unwinds a scratch only for an interruption that
    /// `ends_the_computation`, and a limit is deliberately not one, so the
    /// continuation and the stack are left standing exactly as they were.
    /// Calling `run` again continues from there.
    ///
    ///    The limits are absolute, not per call: `Limit::Step` is raised when
    /// the agent's *cumulative* step count reaches `limits.step`. To run
    /// another thousand steps, ask for `counts().step + 1000`. `step_budget`
    /// is that arithmetic, so a caller does not have to remember it.
    pub fn eval_prog_limited(
        &mut self,
        prog: Prog,
        limits: &Limits,
    ) -> Result<Value_, Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.cont = Cont::Decs(prog.vec);
        self.run(limits)
    }

    /// Limits that stop `more` steps from now, wherever the count stands.
    ///
    /// The VM's step limit is a mark to reach, not an allowance to spend, so
    /// a caller resuming in chunks would otherwise have to add the current
    /// total to every request -- and would stop immediately if it forgot.
    pub fn step_budget(&self, more: usize) -> Limits {
        Limits::none().step(self.agent.counts.step.saturating_add(more))
    }

    /// How many steps this agent has taken, over its whole life.
    pub fn steps_taken(&self) -> usize {
        self.agent.counts.step
    }

    /// Evaluate a new program fragment, assuming agent is idle.
    pub fn eval_exp(&mut self, e: Exp_) -> Result<Value_, Interruption> {
        self.assert_idle_agent()
            .map_err(Interruption::EvalInitError)?;
        self.agent.active.cont = Cont::Exp_(e, Vector::new());
        self.run(&Limits::none())
    }

    #[inline]
    pub fn dealloc(&mut self, pointer: &Pointer) -> Option<Value_> {
        self.store().dealloc(&pointer.local)
    }

    #[inline]
    pub fn define(&mut self, id: impl ToId, value: impl Into<Value_>) {
        let value = value.into();
        self.env().insert(id.to_id(), value);
    }

    #[inline]
    pub fn get_var(&mut self, id: impl ToId) -> Option<Value_> {
        match self.env().get(&id.to_id()) {
            None => None,
            Some(v) => Some(v.clone()),
        }
    }

    #[inline]
    pub fn assign_alloc(&mut self, id: impl ToId, value: impl Into<Value_>) -> Pointer {
        let pointer = self.alloc(value);
        self.define(id, Value::Pointer(pointer.fast_clone()).share());
        pointer
    }
}
