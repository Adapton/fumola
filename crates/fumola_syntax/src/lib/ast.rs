use crate::shared::{Share, Shared};
// use crate::ToMotoko;
use im_rc::Vector;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

/// A "located `X`" has a source location of type `Source`.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Loc<X>(pub X, pub Source);

impl PartialOrd for Id {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.string.partial_cmp(&other.string)
    }
}

impl Ord for Id {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.string.cmp(&other.string)
    }
}

impl<X: std::fmt::Debug> std::fmt::Debug for Loc<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

pub type Node<X> = Shared<NodeData<X>>;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct NodeData<X>(pub X, pub Source);

impl<X: std::fmt::Debug> std::fmt::Debug for NodeData<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}@{:?}>", self.0, self.1)
    }
}

impl<X> Loc<X> {
    pub fn map<F: Fn(X) -> T, T>(self, map_fn: F) -> Loc<T> {
        Loc(map_fn(self.0), self.1)
    }
}

impl<X: Clone> Node<X> {
    pub fn without_source(value: X) -> Self {
        NodeData(value, Source::Unknown).share()
    }

    pub fn map_node<F: Fn(&X) -> T, T: Clone>(self, map_fn: F) -> Node<T> {
        NodeData(map_fn(&self.0), self.1.clone()).share()
    }
}

pub type Span = Range<usize>;

impl<X: Clone> NodeData<X> {
    pub fn new(x: X, s: Source) -> Self {
        NodeData(x, s)
    }
    pub fn eval(x: X) -> Self {
        NodeData(x, Source::Evaluation)
    }
    pub fn data_ref(&self) -> &X {
        &self.0
    }
    pub fn data_clone(&self) -> X {
        self.0.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(tag = "source_type")]
pub enum Source {
    Known(Rc<SourceKnown>),
    // ExpStep { source: Shared<Source> },
    Unknown,
    Evaluation,
    CoreInit,
    CoreCreateActor,
    CoreUpgradeActor,
    CoreSetModule,
    CoreCall,
    ImportPrim,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct SourceKnown {
    pub span: Span,
    pub line: usize,
    pub col: usize,
}

impl Source {
    pub fn span(&self) -> Option<Span> {
        use Source::*;
        match self {
            Known(k) => Some(k.span.clone()),
            _ => None,
        }
    }
    pub fn expand(&self, other: &Source) -> Source {
        use Source::*;
        match (self, other) {
            (Unknown, Unknown) => Source::Unknown,
            (Known(k1), Known(k2)) => Known(Rc::new(SourceKnown {
                span: k1.span.start..k2.span.end,
                line: k1.line,
                col: k2.col,
            })),
            (_, Unknown) => self.clone(),
            (Unknown, _) => other.clone(),
            (CoreInit, _) => todo!(),
            (_, CoreInit) => todo!(),
            (Evaluation, _) => todo!(),
            (_, Evaluation) => todo!(),
            _ => todo!(),
            // (ExpStep { .. }, _) => todo!(),
            // (_, ExpStep { .. }) => todo!(),
        }
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            //Source::Known { line, col, .. } => write!(f, "{}:{}", line, col),
            Source::Known(k) => {
                write!(f, "{}..{} @ {}:{}", k.span.start, k.span.end, k.line, k.col)
            }
            // Source::ExpStep { source } => {
            //     write!(f, "ExpStep({})", source)
            // }
            Source::Unknown => write!(f, "(unknown source)"),
            Source::Evaluation => write!(f, "(evaluation)"),
            Source::CoreInit => write!(f, "(full program, via core init)"),
            Source::CoreCreateActor => write!(f, "(Core.create_actor())"),
            Source::CoreUpgradeActor => write!(f, "(Core.upgrade_actor())"),
            Source::CoreCall => write!(f, "(Core.call())"),
            Source::CoreSetModule => write!(f, "(Core.set_module())"),
            Source::ImportPrim => write!(f, "(import ⛔)"),
        }
    }
}

impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::default::Default for Source {
    fn default() -> Self {
        Source::Unknown
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Delim<X: Clone> {
    pub vec: Vector<X>,
    pub has_trailing: bool,
}

impl<X: Clone> Delim<X> {
    pub fn new() -> Self {
        Delim {
            vec: im_rc::vector![],
            has_trailing: false,
        }
    }
    pub fn append(&self, other: &Self) -> Self {
        let mut vec = self.vec.clone();
        vec.append(other.vec.clone());
        Delim {
            vec,
            has_trailing: other.has_trailing,
        }
    }
    pub fn one(x: X) -> Self {
        let vec = im_rc::vector![x];
        Delim {
            vec,
            has_trailing: false,
        }
    }
    pub fn from(vec: Vec<X>) -> Self {
        Delim {
            vec: vec.into(),
            has_trailing: false,
        }
    }
}
impl<X: Clone> Default for Delim<X> {
    fn default() -> Self {
        Self::new()
    }
}

pub type Literal_ = Node<Literal>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Literal {
    Null,
    Bool(bool),
    Unit,
    Nat(String),
    // Int(String),
    Float(String),
    Char(String), // includes quotes
    Text(String), // includes quotes
    Blob(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ObjSort {
    Object,
    Actor,
    Module,
}

pub type TypId = Id;
pub type TypId_ = Node<TypId>;

pub type Decs = Delim<Dec_>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum CasesPos {
    Cases(Cases),
    Unquote(Unquote_),
}

impl CasesPos {
    pub fn cases<'a>(&'a self) -> &'a Cases {
        match self {
            CasesPos::Cases(c) => c,
            CasesPos::Unquote(_) => panic!(),
        }
    }
}

impl DecFieldsPos {
    pub fn dec_fields<'a>(&'a self) -> &'a DecFields {
        match self {
            DecFieldsPos::DecFields(dfs) => dfs,
            DecFieldsPos::Unquote(_) => panic!(),
        }
    }
}

pub type Cases = Delim<Case_>;

pub type Prog = Decs;

pub type Dec_ = Node<Dec>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Dec {
    Attrs(Attrs, Dec_),
    Exp(Exp_),
    Let(Pat_, Exp_),
    LetImport(Pat_, Sugar, String),
    LetModule(Option<IdPos_>, Sugar, DecFieldsPos),
    LetActor(Option<IdPos_>, Sugar, DecFieldsPos),
    LetObject(Option<IdPos_>, Sugar, DecFieldsPos),
    Func(Function),
    Var(Pat_, Exp_),
    Type(TypId_, Option<TypeBinds>, Type_),
    Class(Class),
}

impl Dec {
    pub fn with_attrs(attrs: Node<Option<(&str, Attrs)>>, dec: Dec_) -> Dec_ {
        match &attrs.0 {
            None => dec,
            Some(a) => NodeData(Dec::Attrs(a.1.clone(), dec), attrs.1.clone()).into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Class {
    pub shared: Option<SortPat>,
    pub sort: Option<ObjSort>,
    pub typ_id: Option<TypId_>,
    pub binds: Option<TypeBinds>,
    pub input: Pat_,
    pub typ: Option<Type_>,
    pub name: Option<Id_>,
    pub fields: DecFieldsPos,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct SortPat {
    pub shared_keyword: bool, // explicit 'shared' keyword
    pub sort: SharedSort,
    pub pat: Pat_,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum SharedSort {
    Query,
    Update,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum BindSort {
    Scope,
    Type,
}

pub type TypeBind_ = Node<TypeBind>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct TypeBind {
    pub var: Id_,
    pub sort: BindSort,
    pub bound: Option<Type_>,
}

/// Mutability setting, for arrays, record fields and lexically-scoped bindings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Mut {
    Const,
    Var,
}

pub type DecFields = Delim<DecField_>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum DecFieldsPos {
    DecFields(DecFields),
    Unquote(Unquote_),
}

pub type TypeBinds = Delim<TypeBind_>;
pub type ExpFields = Delim<ExpField_>;
pub type PatFields = Delim<PatField_>;
pub type TypeFields = Delim<TypeField_>;

pub type Case_ = Node<Case>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Case {
    pub pat: Pat_,
    pub exp: Exp_,
}

pub type ExpField_ = Node<ExpField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ExpField {
    pub mut_: Mut,
    pub id: IdPos_,
    pub typ: Option<Type_>,
    pub exp: Option<Exp_>,
}

impl ExpField {
    pub fn exp_(&self) -> Exp_ {
        match self.exp.clone() {
            Some(e) => e,
            None => NodeData(Exp::Var(self.id.clone()), self.id.1.clone()).share(),
        }
    }
}

pub type DecField_ = Node<DecField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct DecField {
    pub attrs: Option<Attrs>,
    pub dec: Dec_,
    pub vis: Option<Vis_>,
    pub stab: Option<Stab_>,
}

pub type PatField_ = Node<PatField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct PatField {
    pub id: IdPos_,
    pub pat: Option<Pat>,
}

pub type TypeField_ = Node<TypeField>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum TypeField {
    Val(ValTypeField),
    Type, // to do -- represent AST here.
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ValTypeField {
    pub mut_: Mut,
    pub id: Id_,
    pub typ: Type_,
}

pub type TypeTag_ = Node<TypeTag>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct TypeTag {
    pub id: Id_,
    pub typ: Option<Type_>,
}

pub type Attrs = Delim<Attr_>;

pub type Attr_ = Node<Attr>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Attr {
    Id(Id_),
    Call(Id_, Delim<Attr_>),
    Field(Id_, Attr_),
    Literal(Literal_),
}

pub type Vis_ = Node<Vis>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Vis {
    Public(Option<Id_>),
    Private,
    System,
}

impl Vis {
    pub fn is_public(&self) -> bool {
        match self {
            Vis::Public(..) => true,
            _ => false,
        }
    }
}

pub type Stab_ = Node<Stab>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Stab {
    Stable,
    Flexible,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ResolvedImport {
    Unresolved,
    Lib(String),
    Candid { path: String, bytes: Vec<u8> },
    Prim,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, Default)]
pub struct Sugar(pub bool);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum PrimType {
    Null,
    Unit,
    Bool,
    Nat,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Text,
    Char,
    Principal,
}

impl PrimType {
    pub fn from_id(text: &str) -> Option<PrimType> {
        use PrimType::*;
        Some(match text {
            "Bool" => Bool,
            "Nat" => Nat,
            "Nat8" => Nat8,
            "Nat16" => Nat16,
            "Nat32" => Nat32,
            "Nat64" => Nat64,
            "Int" => Int,
            "Int8" => Int8,
            "Int16" => Int16,
            "Int32" => Int32,
            "Int64" => Int64,
            "Principal" => Principal,
            "Text" => Text,
            _ => None?,
        })
    }

    pub fn from_text(text: &str) -> Option<PrimType> {
        let id = format!("{}", &text[1..text.len() - 1]);
        Self::from_id(&id)
    }
}

pub type Type_ = Node<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Type {
    Item(Id_, Type_),
    Path(TypePath, Option<Delim<Type_>>),
    Prim(PrimType),
    Object(ObjSort, Delim<TypeField_>),
    Array(Mut, Type_),
    Optional(Type_),
    Variant(Delim<TypeTag_>),
    Tuple(Delim<Type_>),
    Function(Option<SortPat>, Option<TypeBinds>, Type_, Type_),
    // Async(Type_, Type_), -- to do -- use scope variables
    Async(Type_),
    And(Type_, Type_),
    Or(Type_, Type_),
    Paren(Type_),
    Unknown(String),
    Known(Id_, Type_),
}

impl Type {
    pub fn prim(t: &str) -> Type {
        PrimType::from_text(t)
            .map(Type::Prim)
            .unwrap_or_else(|| Type::Unknown(t.to_string()))
    }

    pub fn id_type_args(id: Id_, ta: Option<Delim<Type_>>) -> Type {
        // assuming here that no primitive types are parameterized by type args.
        if let None = ta {
            // let t = PrimType::from_text(id.0.as_str());
            let t = PrimType::from_id(id.0.as_str());
            t.map(Type::Prim)
                .unwrap_or_else(|| Type::Path(TypePath::Id(id), None))
        } else {
            Type::Path(TypePath::Id(id), ta)
        }
    }
}

pub type TypePath_ = Node<TypePath>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum TypePath {
    Id(Id_),
    Dot(TypePath_, Id_),
}

pub type Inst = Delim<Type_>;

// Convention: Foo_ = Located<Box<Foo>
// where Foo is an enum for an AST subsort, like Exp.

pub type Exp_ = Node<Exp>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Function {
    pub name: Option<IdPos_>,
    pub shared: Option<SortPat>,
    pub binds: Option<TypeBinds>,
    pub input: Pat_,
    pub output: Option<Type_>,
    #[serde(default = "Default::default")]
    pub sugar: Sugar,
    pub exp: Exp_,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ProjIndex {
    Usize(usize),
    // when the parser gets confused by double-indexes, "((a,b),c).0.1"
    FloatLike(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum QuotedAst {
    Empty,
    Id(Id),
    Id_(Id_),
    Literal(Literal_),
    TupleExps(Delim<Exp_>),
    TuplePats(Delim<Pat_>),
    RecordExps(ExpObjectBody),
    RecordPats(PatFields),
    Cases(Cases),
    Decs(Decs),
    DecFields(DecFields),
    Types(Delim<Type_>),
    Attrs(Attrs),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum AdaptonNavDim {
    Space,
    Time,
}
pub type AdaptonNavDim_ = Node<AdaptonNavDim>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum AdaptonNav {
    Goto(Option<AdaptonNavDim_>, Exp_),
    Within(Option<AdaptonNavDim_>, Exp_),
}
pub type AdaptonNav_ = Node<AdaptonNav>;

pub type ExpObjectBody = (Option<Delim<Exp_>>, Option<ExpFields>);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Exp {
    Hole,
    Prim(Result<PrimFunction, String>),
    Var(IdPos_),
    Literal(Literal),
    ActorUrl(Exp_),
    Un(UnOp, Exp_),
    Bin(Exp_, BinOp, Exp_),
    Rel(Exp_, RelOp, Exp_),
    Show(Exp_),
    ToCandid(Delim<Exp_>),
    FromCandid(Exp_),
    Tuple(Delim<Exp_>),
    Proj(Exp_, ProjIndex),
    Opt(Exp_),
    DoOpt(Exp_),
    DoAdaptonNav(Vector<AdaptonNav_>, Exp_),
    DoAdaptonPutForceThunk(Exp_, Exp_),
    Bang(Exp_),
    ObjectBlock(ObjSort, DecFieldsPos),
    Object(ExpObjectBody),
    Variant(IdPos_, Option<Exp_>),
    Dot(Exp_, IdPos_),
    Assign(Exp_, Exp_),
    BinAssign(Exp_, BinOp, Exp_),
    Array(Mut, Delim<Exp_>),
    Index(Exp_, Exp_),
    Function(Function),
    Call(Exp_, Option<Inst>, Exp_),
    Block(Decs),
    Do(Exp_),
    Not(Exp_),
    And(Exp_, Exp_),
    Or(Exp_, Exp_),
    If(Exp_, Exp_, Option<Exp_>),
    Switch(Exp_, CasesPos),
    While(Exp_, Exp_),
    Loop(Exp_, Option<Exp_>),
    For(Pat_, Exp_, Exp_),
    Label(Id_, Option<Type_>, Exp_),
    Break(Id_, Option<Exp_>),
    Return(Option<Exp_>),
    Debug(Exp_),
    DebugShow(Exp_),
    Async(Exp_),
    AsyncStar(Exp_),
    Await(Exp_),
    AwaitStar(Exp_),
    Assert(Exp_),
    Annot(BinAnnotWasHoisted, Exp_, Type_),
    //Annot(Exp_, Type_),
    //Import(Id_, ResolvedImport),
    Import(String),
    Throw(Exp_),
    Try(Exp_, Case_),
    Ignore(Exp_),
    Paren(Exp_),
    QuotedAst(QuotedAst),
    Unquote(Exp_),
    Force(Exp_),
    Thunk(Exp_),
    GetAdaptonPointer(Exp_),
}

impl Exp {
    pub fn id_var(id: Id_) -> Exp {
        let source = id.1.clone();
        Exp::Var(NodeData(IdPos { id, unquote: false }, source).share())
    }

    pub fn object_body(&self) -> ExpObjectBody {
        match self {
            Exp::Object(body) => body.clone(),
            _ => panic!(),
        }
    }

    pub fn obj_field_fields(f1: ExpField_, fs: Option<ExpFields>) -> Exp {
        match fs {
            None => Exp::Object((None, Some(Delim::one(f1)))),
            Some(mut fs) => {
                fs.vec.push_front(f1);
                Exp::Object((None, Some(fs)))
            }
        }
    }
    pub fn obj_id_fields(id: IdPos_, fields: ExpFields) -> Exp {
        let field1_source = id.1.clone();
        let exp = Some(NodeData(Exp::Var(id.clone()), id.1.clone()).share());
        let field1 = NodeData(
            ExpField {
                mut_: Mut::Const,
                id,
                exp,
                typ: None,
            },
            field1_source,
        )
        .share();
        let mut fields = fields;
        fields.vec.push_front(field1);
        Exp::Object((None, Some(fields)))
    }
    pub fn obj_base_bases(base1: Exp_, bases: Option<Delim<Exp_>>, efs: Option<ExpFields>) -> Exp {
        match (bases, efs) {
            (None, None) => match &base1.0 {
                Exp::Var(x) => Exp::obj_id_fields(x.clone(), Delim::new()),
                _ => unimplemented!("parse error"),
            },
            (None, efs) => Exp::Object((Some(Delim::one(base1)), efs)),
            (Some(mut bs), efs) => {
                bs.vec.push_front(base1);
                Exp::Object((Some(bs), efs))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct BinAnnotWasHoisted(pub bool);

pub type Pat_ = Node<Pat>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Pat {
    Wild,
    Var(Id_),
    Literal(Literal),
    UnOpLiteral(UnOp_, Literal_),
    Tuple(Delim<Pat_>),
    Object(PatFields),
    Optional(Pat_),
    Variant(IdPos_, Option<Pat_>),
    Or(Pat_, Pat_),
    AnnotPat(Pat_, Type_),
    Annot(Type_),
    Paren(Pat_),
    Unquote(Unquote),
    // used by the VM to pattern-match values.
    TempVar(u16),
}

pub type UnOp_ = Node<UnOp>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, PartialOrd)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

// time and space are not keywords, which means we can still use
// them as identifiers in Motoko modules that we use.
// this hack makes that flexibility possible.
pub fn into_adapton_nav_dim(id: IdPos_) -> Option<AdaptonNavDim_> {
    match id.0.id.0.string.as_str() {
        "time" => Some(NodeData::new(AdaptonNavDim::Time, id.1.clone()).into()),
        "space" => Some(NodeData::new(AdaptonNavDim::Space, id.1.clone()).into()),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, PartialOrd)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Xor,
    ShL,
    ShR,
    RotL,
    RotR,
    WAdd,
    WSub,
    WMul,
    WPow,
    Cat,
    BitOr,
    BitAnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Unquote {
    pub id: Id_,
}

pub type Unquote_ = Node<Unquote>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct IdPos {
    pub unquote: bool,
    pub id: Id_,
}

impl IdPos {
    pub fn from_id(i: &Id_) -> Self {
        IdPos {
            unquote: false,
            id: i.clone(),
        }
    }
    pub fn id_(&self) -> Id_ {
        assert_eq!(self.unquote, false);
        self.id.clone()
    }
    pub fn id(&self) -> Id {
        self.id_().0.clone()
    }
    pub fn id_ref<'a>(&'a self) -> &'a Id {
        &self.id.as_ref().0
    }
}

pub type IdPos_ = Node<IdPos>;

#[derive(Clone)]
pub struct Id {
    pub string: Shared<String>,
    // todo -- use https://docs.rs/crate/nohash-hasher/latest to avoid
    // hashing anything (not even the pre-computed hash) and just
    // *use* the pre-computed hash.
    pub hash: u64,
}
pub type Id_ = Node<Id>;

impl Id {
    pub fn new(s: String) -> Self {
        let hash: u64 = {
            let mut h = DefaultHasher::new();
            s.hash(&mut h);
            h.finish()
        };
        Id {
            // to do -- memoize the strings, and avoid duplicate copies.
            string: Shared::new(s),
            hash,
        }
    }
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
    pub fn to_string(&self) -> String {
        self.string.to_string()
    }
    pub fn share(self) -> Shared<Self> {
        crate::shared::Shared::new(self)
    }
}

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.string)
    }
}

// TODO: see whether this is faster than the default `PartialEq` impl
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.string == other.string
    }

    fn ne(&self, other: &Self) -> bool {
        self.hash != other.hash || self.string != other.string
    }
}
impl Eq for Id {}

impl Hash for Id {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serde::Serialize::serialize(&self.string, serializer)
    }
}

impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde::Deserialize::deserialize(deserializer).map(Id::new)
    }
}

pub trait ToId {
    fn to_id(self) -> Id;
}

impl ToId for Id {
    fn to_id(self) -> Id {
        self
    }
}

impl ToId for &str {
    fn to_id(self) -> Id {
        Id::new(self.to_string())
    }
}

impl ToId for String {
    fn to_id(self) -> Id {
        Id::new(self)
    }
}

// hoist right-sided type annotation;
// fixes parse tree to respect binary operator precedence.
pub fn hoist_right_type_annotation(e: Exp) -> Exp {
    // to do
    match e {
        Exp::Bin(e1, binop, e2) => {
            if let Exp::Annot(BinAnnotWasHoisted(false), e3, t) = e2.0.clone() {
                let s = e1.1.expand(&e3.1);
                Exp::Annot(
                    BinAnnotWasHoisted(true),
                    NodeData(Exp::Bin(e1, binop, e3), s).share(),
                    t,
                )
            } else {
                Exp::Bin(e1, binop, e2)
            }
        }
        // to do -- more cases as needed (Rel, And, Or)
        _ => e,
    }
}

pub fn source_from_decs(decs: &im_rc::Vector<Dec_>) -> Source {
    if decs.is_empty() {
        Source::Unknown
    } else {
        let first = decs.front().unwrap().1.clone();
        match decs.back() {
            None => first,
            Some(back) => first.expand(&back.1),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum PrimFunction {
    AdaptonReset,
    AdaptonNow,
    AdaptonHere,
    AdaptonSpace,
    AdaptonTime,
    AdaptonPointer,
    AdaptonPeek,
    AdaptonPoke,
    AtSignVar(String),
    DebugPrint,
    NatToText,
    SymbolLevel,
    WriteFile,
    RustDebugText,
    ReifyValue,
    ReflectValue,
    ReifyCore,
    ReflectCore,
    Collection(CollectionFunction),
}

impl PrimFunction {
    pub fn resolve(name: String) -> Result<PrimFunction, String> {
        use CollectionFunction::*;
        use PrimFunction::*;
        Ok(match name.as_str() {
            "\"adaptonNow\"" => AdaptonNow,
            "\"adaptonReset\"" => AdaptonReset,
            "\"adaptonHere\"" => AdaptonHere,
            "\"adaptonTime\"" => AdaptonTime,
            "\"adaptonSpace\"" => AdaptonSpace,
            "\"adaptonPointer\"" => AdaptonPointer,
            "\"adaptonPeek\"" => AdaptonPeek,
            "\"adaptonPoke\"" => AdaptonPoke,
            "\"print\"" => DebugPrint,
            "\"natToText\"" => NatToText,
            "\"symbolLevel\"" => SymbolLevel,
            "\"hashMapNew\"" => Collection(HashMap(HashMapFunction::New)),
            "\"hashMapPut\"" => Collection(HashMap(HashMapFunction::Put)),
            "\"hashMapGet\"" => Collection(HashMap(HashMapFunction::Get)),
            "\"hashMapRemove\"" => Collection(HashMap(HashMapFunction::Remove)),
            "\"fastRandIterNew\"" => Collection(FastRandIter(FastRandIterFunction::New)),
            "\"fastRandIterNext\"" => Collection(FastRandIter(FastRandIterFunction::Next)),
            "\"writeFile\"" => WriteFile,
            "\"rustDebugText\"" => RustDebugText,
            "\"reifyValue\"" => ReifyValue,
            "\"reflectValue\"" => ReflectValue,
            "\"reifyCore\"" => ReifyCore,
            "\"reflectCore\"" => ReflectCore,
            _ => Err(name.to_string())?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum CollectionFunction {
    HashMap(HashMapFunction),
    FastRandIter(FastRandIterFunction),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum HashMapFunction {
    New,
    Put,
    Get,
    Remove,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum FastRandIterFunction {
    New,
    Next,
}
