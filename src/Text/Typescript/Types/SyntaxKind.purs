module Text.Typesript.Types.SyntaxKind (module Text.Typesript.Types.SyntaxKind) where

import Data.Int.Bits (shl -- Bitwise shift left
                     , (.|.) -- Bitwise or
                     )

foreign import getSyntaxKindForInt :: Int -> String

data SyntaxKind = SyntaxKindSub SubSyntaxKind | SyntaxKindMarker SubSyntaxKindMarker

-- | token > SyntaxKind.Identifier => token is a keyword
-- | Also If you add a new SyntaxKind be sure to keep the `Markers` section at the bottom in sync
data SubSyntaxKind =
    Unknown
  | EndOfFileToken
  | SingleLineCommentTrivia
  | MultiLineCommentTrivia
  | NewLineTrivia
  | WhitespaceTrivia
  -- | We detect and preserve #! on the first line
  | ShebangTrivia
  -- | We detect and provide better error recovery when we encounter a git merge marker.  This
  -- | allows us to edit files with git-conflict markers in them in a much more pleasant manner.
  | ConflictMarkerTrivia
  -- | Literals
  | NumericLiteral
  | BigIntLiteral
  | StringLiteral
  | JsxText
  | JsxTextAllWhiteSpaces
  | RegularExpressionLiteral
  | NoSubstitutionTemplateLiteral
  -- | Pseudo-literals
  | TemplateHead
  | TemplateMiddle
  | TemplateTail
  -- | Punctuation
  | OpenBraceToken
  | CloseBraceToken
  | OpenParenToken
  | CloseParenToken
  | OpenBracketToken
  | CloseBracketToken
  | DotToken
  | DotDotDotToken
  | SemicolonToken
  | CommaToken
  | QuestionDotToken
  | LessThanToken
  | LessThanSlashToken
  | GreaterThanToken
  | LessThanEqualsToken
  | GreaterThanEqualsToken
  | EqualsEqualsToken
  | ExclamationEqualsToken
  | EqualsEqualsEqualsToken
  | ExclamationEqualsEqualsToken
  | EqualsGreaterThanToken
  | PlusToken
  | MinusToken
  | AsteriskToken
  | AsteriskAsteriskToken
  | SlashToken
  | PercentToken
  | PlusPlusToken
  | MinusMinusToken
  | LessThanLessThanToken
  | GreaterThanGreaterThanToken
  | GreaterThanGreaterThanGreaterThanToken
  | AmpersandToken
  | BarToken
  | CaretToken
  | ExclamationToken
  | TildeToken
  | AmpersandAmpersandToken
  | BarBarToken
  | QuestionToken
  | ColonToken
  | AtToken
  | QuestionQuestionToken
  -- | Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds. */
  | BacktickToken
  -- | Assignments
  | EqualsToken
  | PlusEqualsToken
  | MinusEqualsToken
  | AsteriskEqualsToken
  | AsteriskAsteriskEqualsToken
  | SlashEqualsToken
  | PercentEqualsToken
  | LessThanLessThanEqualsToken
  | GreaterThanGreaterThanEqualsToken
  | GreaterThanGreaterThanGreaterThanEqualsToken
  | AmpersandEqualsToken
  | BarEqualsToken
  | BarBarEqualsToken
  | AmpersandAmpersandEqualsToken
  | QuestionQuestionEqualsToken
  | CaretEqualsToken
  -- | Identifiers and PrivateIdentifiers
  | Identifier
  | PrivateIdentifier
  -- | Reserved words
  | BreakKeyword
  | CaseKeyword
  | CatchKeyword
  | ClassKeyword
  | ConstKeyword
  | ContinueKeyword
  | DebuggerKeyword
  | DefaultKeyword
  | DeleteKeyword
  | DoKeyword
  | ElseKeyword
  | EnumKeyword
  | ExportKeyword
  | ExtendsKeyword
  | FalseKeyword
  | FinallyKeyword
  | ForKeyword
  | FunctionKeyword
  | IfKeyword
  | ImportKeyword
  | InKeyword
  | InstanceOfKeyword
  | NewKeyword
  | NullKeyword
  | ReturnKeyword
  | SuperKeyword
  | SwitchKeyword
  | ThisKeyword
  | ThrowKeyword
  | TrueKeyword
  | TryKeyword
  | TypeOfKeyword
  | VarKeyword
  | VoidKeyword
  | WhileKeyword
  | WithKeyword
  -- | Strict mode reserved words
  | ImplementsKeyword
  | InterfaceKeyword
  | LetKeyword
  | PackageKeyword
  | PrivateKeyword
  | ProtectedKeyword
  | PublicKeyword
  | StaticKeyword
  | YieldKeyword
  -- | Contextual keywords
  | AbstractKeyword
  | AsKeyword
  | AssertsKeyword
  | AnyKeyword
  | AsyncKeyword
  | AwaitKeyword
  | BooleanKeyword
  | ConstructorKeyword
  | DeclareKeyword
  | GetKeyword
  | InferKeyword
  | IsKeyword
  | KeyOfKeyword
  | ModuleKeyword
  | NamespaceKeyword
  | NeverKeyword
  | ReadonlyKeyword
  | RequireKeyword
  | NumberKeyword
  | ObjectKeyword
  | SetKeyword
  | StringKeyword
  | SymbolKeyword
  | TypeKeyword
  | UndefinedKeyword
  | UniqueKeyword
  | UnknownKeyword
  | FromKeyword
  | GlobalKeyword
  | BigIntKeyword
  | OfKeyword -- | LastKeyword and LastToken and LastContextualKeyword

  -- | Parse tree nodes

  -- | Names
  | QualifiedName
  | ComputedPropertyName
  -- | Signature elements
  | TypeParameter
  | Parameter
  | Decorator
  -- | TypeMember
  | PropertySignature
  | PropertyDeclaration
  | MethodSignature
  | MethodDeclaration
  | Constructor
  | GetAccessor
  | SetAccessor
  | CallSignature
  | ConstructSignature
  | IndexSignature
  -- | Type
  | TypePredicate
  | TypeReference
  | FunctionType
  | ConstructorType
  | TypeQuery
  | TypeLiteral
  | ArrayType
  | TupleType
  | OptionalType
  | RestType
  | UnionType
  | IntersectionType
  | ConditionalType
  | InferType
  | ParenthesizedType
  | ThisType
  | TypeOperator
  | IndexedAccessType
  | MappedType
  | LiteralType
  | NamedTupleMember
  | ImportType
  -- | Binding patterns
  | ObjectBindingPattern
  | ArrayBindingPattern
  | BindingElement
  -- | Expression
  | ArrayLiteralExpression
  | ObjectLiteralExpression
  | PropertyAccessExpression
  | ElementAccessExpression
  | CallExpression
  | NewExpression
  | TaggedTemplateExpression
  | TypeAssertionExpression
  | ParenthesizedExpression
  | FunctionExpression
  | ArrowFunction
  | DeleteExpression
  | TypeOfExpression
  | VoidExpression
  | AwaitExpression
  | PrefixUnaryExpression
  | PostfixUnaryExpression
  | BinaryExpression
  | ConditionalExpression
  | TemplateExpression
  | YieldExpression
  | SpreadElement
  | ClassExpression
  | OmittedExpression
  | ExpressionWithTypeArguments
  | AsExpression
  | NonNullExpression
  | MetaProperty
  | SyntheticExpression

  -- | Misc
  | TemplateSpan
  | SemicolonClassElement
  -- | Element
  | Block
  | EmptyStatement
  | VariableStatement
  | ExpressionStatement
  | IfStatement
  | DoStatement
  | WhileStatement
  | ForStatement
  | ForInStatement
  | ForOfStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | SwitchStatement
  | LabeledStatement
  | ThrowStatement
  | TryStatement
  | DebuggerStatement
  | VariableDeclaration
  | VariableDeclarationList
  | FunctionDeclaration
  | ClassDeclaration
  | InterfaceDeclaration
  | TypeAliasDeclaration
  | EnumDeclaration
  | ModuleDeclaration
  | ModuleBlock
  | CaseBlock
  | NamespaceExportDeclaration
  | ImportEqualsDeclaration
  | ImportDeclaration
  | ImportClause
  | NamespaceImport
  | NamedImports
  | ImportSpecifier
  | ExportAssignment
  | ExportDeclaration
  | NamedExports
  | NamespaceExport
  | ExportSpecifier
  | MissingDeclaration

  -- | Module references
  | ExternalModuleReference

  -- | JSX
  | JsxElement
  | JsxSelfClosingElement
  | JsxOpeningElement
  | JsxClosingElement
  | JsxFragment
  | JsxOpeningFragment
  | JsxClosingFragment
  | JsxAttribute
  | JsxAttributes
  | JsxSpreadAttribute
  | JsxExpression

  -- | Clauses
  | CaseClause
  | DefaultClause
  | HeritageClause
  | CatchClause

  -- | Property assignments
  | PropertyAssignment
  | ShorthandPropertyAssignment
  | SpreadAssignment

  -- | Enum
  | EnumMember
  -- | Unparsed
  | UnparsedPrologue
  | UnparsedPrepend
  | UnparsedText
  | UnparsedInternalText
  | UnparsedSyntheticReference

  -- | Top-level nodes
  | SourceFile
  | Bundle
  | UnparsedSource
  | InputFiles

  -- | JSDoc nodes
  | JSDocTypeExpression
  -- | The * type
  | JSDocAllType
  -- | The ? type
  | JSDocUnknownType
  | JSDocNullableType
  | JSDocNonNullableType
  | JSDocOptionalType
  | JSDocFunctionType
  | JSDocVariadicType
  -- | https://jsdoc.app/about-namepaths.html
  | JSDocNamepathType
  | JSDocComment
  | JSDocTypeLiteral
  | JSDocSignature
  | JSDocTag
  | JSDocAugmentsTag
  | JSDocImplementsTag
  | JSDocAuthorTag
  | JSDocDeprecatedTag
  | JSDocClassTag
  | JSDocPublicTag
  | JSDocPrivateTag
  | JSDocProtectedTag
  | JSDocReadonlyTag
  | JSDocCallbackTag
  | JSDocEnumTag
  | JSDocParameterTag
  | JSDocReturnTag
  | JSDocThisTag
  | JSDocTypeTag
  | JSDocTemplateTag
  | JSDocTypedefTag
  | JSDocPropertyTag

  -- | Synthesized list
  | SyntaxList

  -- | Transformation nodes
  | NotEmittedStatement
  | PartiallyEmittedExpression
  | CommaListExpression
  | MergeDeclarationMarker
  | EndOfDeclarationMarker
  | SyntheticReferenceExpression

  -- | Enum value count
  | Count


data SubSyntaxKindMarker =
    FirstAssignment
  | LastAssignment
  | FirstCompoundAssignment
  | LastCompoundAssignment
  | FirstReservedWord
  | LastReservedWord
  | FirstKeyword
  | LastKeyword
  | FirstFutureReservedWord
  | LastFutureReservedWord
  | FirstTypeNode
  | LastTypeNode
  | FirstPunctuation
  | LastPunctuation
  | FirstToken
  | LastToken
  | FirstTriviaToken
  | LastTriviaToken
  | FirstLiteralToken
  | LastLiteralToken
  | FirstTemplateToken
  | LastTemplateToken
  | FirstBinaryOperator
  | LastBinaryOperator
  | FirstStatement
  | LastStatement
  | FirstNode
  | FirstJSDocNode
  | LastJSDocNode
  | FirstJSDocTagNode
  | LastJSDocTagNode
  | FirstContextualKeyword
  | LastContextualKeyword

markerToSubSyntaxKind :: SubSyntaxKindMarker -> SubSyntaxKind
markerToSubSyntaxKind marker = case marker of
  FirstAssignment -> EqualsToken
  LastAssignment -> CaretEqualsToken
  FirstCompoundAssignment -> PlusEqualsToken
  LastCompoundAssignment -> CaretEqualsToken
  FirstReservedWord -> BreakKeyword
  LastReservedWord -> WithKeyword
  FirstKeyword -> BreakKeyword
  LastKeyword -> OfKeyword
  FirstFutureReservedWord -> ImplementsKeyword
  LastFutureReservedWord -> YieldKeyword
  FirstTypeNode -> TypePredicate
  LastTypeNode -> ImportType
  FirstPunctuation -> OpenBraceToken
  LastPunctuation -> CaretEqualsToken
  FirstToken -> Unknown
  LastToken -> markerToSubSyntaxKind LastKeyword
  FirstTriviaToken -> SingleLineCommentTrivia
  LastTriviaToken -> ConflictMarkerTrivia
  FirstLiteralToken -> NumericLiteral
  LastLiteralToken -> NoSubstitutionTemplateLiteral
  FirstTemplateToken -> NoSubstitutionTemplateLiteral
  LastTemplateToken -> TemplateTail
  FirstBinaryOperator -> LessThanToken
  LastBinaryOperator -> CaretEqualsToken
  FirstStatement -> VariableStatement
  LastStatement -> DebuggerStatement
  FirstNode -> QualifiedName
  FirstJSDocNode -> JSDocTypeExpression
  LastJSDocNode -> JSDocPropertyTag
  FirstJSDocTagNode -> JSDocTag
  LastJSDocTagNode -> JSDocPropertyTag
  FirstContextualKeyword -> AbstractKeyword
  LastContextualKeyword -> OfKeyword

data NodeFlag =
    None
  | Let
  | Const
  | NestedNamespace
  | Synthesized
  | Namespace
  | OptionalChain
  | ExportContext
  | ContainsThis
  | HasImplicitReturn
  | HasExplicitReturn
  | GlobalAugmentation
  | HasAsyncFunctions
  | DisallowInContext
  | YieldContext
  | DecoratorContext
  | AwaitContext
  | ThisNodeHasError
  | JavaScriptFile
  | ThisNodeOrAnySubNodesHasError
  | HasAggregatedChildData

  -- |These flags will be set when the parser encounters a dynamic import expression or 'import.meta' to avoid
  -- |walking the tree if the flags are not set. However, these flags are just a approximation
  -- |(hence why it's named "PossiblyContainsDynamicImport") because once set, the flags never get cleared.
  -- |During editing, if a dynamic import is removed, incremental parsing will *NOT* clear this flag.
  -- |This means that the tree will always be traversed during module resolution, or when looking for external module indicators.
  -- |However, the removal operation should not occur often and in the case of the
  -- |removal, it is likely that users will add the import anyway.
  -- |The advantage of this approach is its simplicity. For the case of batch compilation,
  -- |we guarantee that users won't have to pay the price of walking the tree if a dynamic import isn't used.
  | PossiblyContainsDynamicImport
  | PossiblyContainsImportMeta

  | JSDoc
  | Ambient
  | InWithStatement
  | JsonFile
  | TypeCached
  | Deprecated
  | BlockScoped

  | ReachabilityCheckFlags
  | ReachabilityAndEmitFlags

  -- | Parsing context flags
  | ContextFlags

  -- | Exclude these flags when parsing a Type
  | TypeExcludesFlags

  -- | Represents all flags that are potentially set once and
  -- | never cleared on SourceFiles which get re-used in between incremental parses.
  -- | See the comment above on `PossiblyContainsDynamicImport` and `PossiblyContainsImportMeta`.
  | PermanentlySetIncrementalFlags

nodeFlagToInt :: NodeFlag -> Int
nodeFlagToInt nodeFlag = case nodeFlag of
    None                -> 0
    Let                 -> 1 `shl` 0  -- |Variable declaration
    Const               -> 1 `shl` 1  -- |Variable declaration
    NestedNamespace     -> 1 `shl` 2  -- |Namespace declaration
    Synthesized         -> 1 `shl` 3  -- |Node was synthesized during transformation
    Namespace           -> 1 `shl` 4  -- |Namespace declaration
    OptionalChain       -> 1 `shl` 5  -- |Chained MemberExpression rooted to a pseudo-OptionalExpression
    ExportContext       -> 1 `shl` 6  -- |Export context (initialized by binding)
    ContainsThis        -> 1 `shl` 7  -- |Interface contains references to "this"
    HasImplicitReturn   -> 1 `shl` 8  -- |If function implicitly returns on one of codepaths (initialized by binding)
    HasExplicitReturn   -> 1 `shl` 9  -- |If function has explicit reachable return on one of codepaths (initialized by binding)
    GlobalAugmentation  -> 1 `shl` 10 -- |Set if module declaration is an augmentation for the global scope
    HasAsyncFunctions   -> 1 `shl` 11 -- |If the file has async functions (initialized by binding)
    DisallowInContext   -> 1 `shl` 12 -- |If node was parsed in a context where 'in-expressions' are not allowed
    YieldContext        -> 1 `shl` 13 -- |If node was parsed in the 'yield' context created when parsing a generator
    DecoratorContext    -> 1 `shl` 14 -- |If node was parsed as part of a decorator
    AwaitContext        -> 1 `shl` 15 -- |If node was parsed in the 'await' context created when parsing an async function
    ThisNodeHasError    -> 1 `shl` 16 -- |If the parser encountered an error when parsing the code that created this node
    JavaScriptFile      -> 1 `shl` 17 -- |If node was parsed in a JavaScript
    ThisNodeOrAnySubNodesHasError  -> 1 `shl` 18 -- |If this node or any of its children had an error
    HasAggregatedChildData  -> 1 `shl` 19 -- |If we've computed data from children and cached it in this node

  -- |These flags will be set when the parser encounters a dynamic import expression or 'import.meta' to avoid
  -- |walking the tree if the flags are not set. However, these flags are just a approximation
  -- |(hence why it's named "PossiblyContainsDynamicImport") because once set, the flags never get cleared.
  -- |During editing, if a dynamic import is removed, incremental parsing will *NOT* clear this flag.
  -- |This means that the tree will always be traversed during module resolution, or when looking for external module indicators.
  -- |However, the removal operation should not occur often and in the case of the
  -- |removal, it is likely that users will add the import anyway.
  -- |The advantage of this approach is its simplicity. For the case of batch compilation,
  -- |we guarantee that users won't have to pay the price of walking the tree if a dynamic import isn't used.
    PossiblyContainsDynamicImport  -> 1 `shl` 20
    PossiblyContainsImportMeta     -> 1 `shl` 21

    JSDoc                          -> 1 `shl` 22 -- |If node was parsed inside jsdoc
    Ambient                        -> 1 `shl` 23 -- |If node was inside an ambient context -- a declaration file, or inside something with the `declare` modifier.
    InWithStatement                -> 1 `shl` 24 -- |If any ancestor of node was the `statement` of a WithStatement (not the `expression`)
    JsonFile                       -> 1 `shl` 25 -- |If node was parsed in a Json
    TypeCached                     -> 1 `shl` 26 -- |If a type was cached for node at any point
    Deprecated                     -> 1 `shl` 27 -- |If has '@deprecated' JSDoc tag
    BlockScoped                    -> nodeFlagToInt Let .|. nodeFlagToInt Const

    ReachabilityCheckFlags         -> nodeFlagToInt HasImplicitReturn .|. nodeFlagToInt HasExplicitReturn
    ReachabilityAndEmitFlags       -> nodeFlagToInt ReachabilityCheckFlags .|. nodeFlagToInt HasAsyncFunctions

  -- | Parsing context flags
    ContextFlags                   -> nodeFlagToInt DisallowInContext .|. nodeFlagToInt YieldContext .|. nodeFlagToInt DecoratorContext .|. nodeFlagToInt AwaitContext .|. nodeFlagToInt JavaScriptFile .|. nodeFlagToInt InWithStatement .|. nodeFlagToInt Ambient

  -- | Exclude these flags when parsing a Type
    TypeExcludesFlags              -> nodeFlagToInt YieldContext .|. nodeFlagToInt AwaitContext

  -- | Represents all flags that are potentially set once and
  -- | never cleared on SourceFiles which get re-used in between incremental parses.
  -- | See the comment above on `PossiblyContainsDynamicImport` and `PossiblyContainsImportMeta`.
    PermanentlySetIncrementalFlags -> nodeFlagToInt PossiblyContainsDynamicImport .|. nodeFlagToInt PossiblyContainsImportMeta
