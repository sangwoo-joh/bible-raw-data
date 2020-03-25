# Clang-Format Internal Structure

## Token

 This structure provides full information about a lexed token. It is
 not intended to be space efficient, it is intended to return as much
 information as possible about each returned token. This is expected
 to be compressed into a smaller form if memory footprint is
 important.

 The parser can create a special "annocation token" representing a
 stream of tokens that were parsed and semantically resolved, e.g.:
 `foo::MyClass<int>` can be represented by a single typename
 annotation token that carries information about the `SourceRange` of
 the tokens and the type object.

 + `unsigned Loc`: The location of the token(`SourceLocation`, the
   offset).
 + `unsigned UintData`: This holds either the length of the token
   text, when a normal token, or the end of the `SourceRange` when an
   annotation token.
 + `void *PtrData`: This is a union of four different pointer types,
   which depends on what type of token this is:
   1. Identifiers, keywords, etc: This is an `IdentifierInfo*`, which
      contains the uniqued identifier spelling.
   2. Literals: `isLiteral()` returns true. This is a pointer to the
      start of the token in a text buffer, which may be dirty (have
      trigraphs / escaped newlines).
   3. Annocations (resolved type names, C++ scopes, etc):
      `isAnnotation()`. This is a pointer to sema-specific data for
      the annotation token.
   4. Eof: This is a pointer to a `Decl`
   5. Other: This is null.
 + `tok::TokenKind Kind`: The actual flavor of token this is.
 + `unsigned short Flags`: Bits we track about this token, members of
   the TokenFlags enum.
   + `enum TokenFlags`: Various flags set per token
     + `StartOfLine`: At start of line or only after whitespace
       (considering the line after macro expansion).
     + `LeadingSpace`: Whitespace exists before this token
       (considering whitespace after macro expansion).
     + `DisabledExpand`: This identifier may never be macro expanded.
     + `NeedsCleaning`: Contained an escaped newline or trigraph.
     + `LeadingEmptyMacro`: Empty macro exists before this token.
     + `HasUDSuffix`: This string or character literal has a
       ud-suffix.
     + `HasUCN`: This identifier contains a UCN.
     + `IgnoredComma`: This comma is not a macro argument separator
       (MS).
     + `StringifiedInMacro`: This string or character literal is
       formed by macro stringizing or charizing operator.
     + `CommaAfterElided`: The comma following this token was elided
       (MS).
     + `IsEditorPlaceHolder`: This identifier is a placeholder.


## FormatToken

 A wrapper around a `Token` storing information about the whitespace
 characters preceding it.


 + `Token tok`: The Token.
 + `unsigned NewlinesBefore`: The number of newlines immediately
   before the Token. This can be used to determine what the user wrote
   in the original code and thereby e.g. leave an empty line between
   two function definitions.
 + `bool HasUnescapedNewline`: Whether there is at least one unescaped
   newline before the Token.
 + `SourceRange WhitespaceRange`: The range of the whitespace
   immediately preceding the Token.
 + `unsigned LastNewlineOffset`: The offset just past the last `\n` in
   this token's leading whitespace (relative to `WhiteSpaceStart`). 0
   if there is no `\n`.
 + `unsigned ColumnWidth`: The width of the non-whitespace pars of the
   token (or its first line for multi-line tokens) in columns. We need
   this to correctly measure number of columns a token spans.
 + `unsigned LastLineColumnWidth`: Contains the width in columns of
   the last line of a multi-line token.
 + `bool IsMultiline`: Whether the token text contains newlines
   (escaped or not).
 + `bool IsFirst`: Indicates that this is the first token of the file.
 + `bool MustBreakBefore`: Whether there must be a line break before
   this token. This happens for example when a preprocessor directive
   ended directly before the token.
 + `StringRef TokenText`: The raw text of the token. Contains the raw
   token text without leading whitespace and without leading escaped
   newlines.
 + `bool IsUnterminatedLiteral`: Set to true if this token is an
   unterminated literal.
 + `BraceBlockKind BlockKind`: Contains the kind of block if this
   token is a brace.
 + `TokenType Type`: Token type.
 + `unsigned SpacesRequiredBefore`: The number of spaces that should
   be inserted before this token.
 + `bool CanBreakBefore`: true if it is allowed to break before this
   token.
 + `bool ClosesTemplateDeclaration`: true if this is the `>` of
   `template<..>`.
 + `unsigned ParameterCount`: Number of parameters, if this is `(`,
   `[` or `<`. This is initialized to 1 as we don't need to
   distinguish functions with 0 parameters from function with 1
   parameter. Thus, we can simply count the number of commas.
 + `unsigned BlockParameterCount`: Number of parameters that are
   nested blocks, if this `(`, `[` or `<`.
 + `tok::TokenKind ParentBracket`: If this is a bracket (`<`, `(`, `[`
   or `{`), contains the kind of the surrounding bracket.
 + `std::unique_ptr<TokenRole> Role`: A token can have a special role
   that can carry extra information about the token's formatting.
 + `ParameterPackingKind PackingKind`: If this is an opening
   parenthesis, how are the parameters packed?
 + `unsigned TotalLength`: The total length of the unwrapped line up
   to and including this token.
 + `unsigned OriginalColumn`: The original 0-based column of this
   token, including expanded tabs. The configured `TabWidth` is used
   as tab width.
 + `unsigned UnbreakableTailLength`: The length of following tokens
   until the next natural split point, or the next token that can be
   broken.
 + `unsigned BindingStrength`: (FIXME: Come up with a 'cleaner'
   concept.) The binding strength of a token. This is a combined value
   of operator precedence, parenthesis nesting, etc.
 + `unsigned NestingLevel`: The nesting level of this token, i.e. the
   number of surrounding `()`, `[]`, `{}` or `<>`.
 + `unsigned IndentLevel`: The indent level of this token. Copied from
   the surrounding line.
 + `unsigned SplitPenalty`: Penalty for inserting a line break before
   this token.
 + `SmallVector<prec::Level, 4> FakeLParens`: Stores the number of
   required fake parentheses and the corresponding operator
   precendence. If multiple fake parentheses start at a token, this
   vector stores them in reverse order, i.e. inner fake parenthesis
   first.
 + `unsigned FakeRParens`: Insert this many `)` after this token for
   correct indentation.
 + `bool StartsBinaryExpression`: true if this token starts a binary
   expression, i.e. has at least one fake l_paren with a precedence
   greater that `prec::Unknown`.
 + `bool EndsBinaryExpression`: true if this token ends a binary
   expression.
 + `unsigned OperatorIndex`: If this is an operator (or `.`/`->`) in a
   sequence of operators with the same precedence, contains the
   0-based operator index.
 + `FormatToken *NextOperator`: If this is an operator (or `.`/`->`)
   in a sequence of operators with the same precedence, points to the
   next operator.
 + `bool PartOfMultiVariableDeclStmt`: Is this token part of a
   `DeclStmt` defining multiple variables? Only set if `Type` ==
   `TT_StartOfName`.
 + `bool ContinuesLineCommentSection`: Does this line comment continue
   a line comment section? Only set to true if `Type` ==
   `TT_LineComment`.
 + `FormatToken *MatchingParen`: If this is a bracket, this points to
   the matching one.
 + `FormatToken *Previous`: The previous token in the unwrapped line.
 + `FormatToken *Next`: The next token in the unwrapped line.
 + `Smallvector<AnnotatedLine *, 1> Children`: If this token starts a
   block, this contains all the unwrapped lines in it.
 + `FormatDecision Decision`: Stores the formatting decision for the
   token once it was made.
 + `bool Finalized`: If true, this token has been fully formatted
   (indented and potentially re-formatted inside), and we do not allow
   further formatting changes.


### `LIST_TOKEN_TYPES`
(Except for Java/JavaScript/Objective-c/c++ Feaures)
 + `ArrayInitializerLSquare`
 + `ArraySubscriptLSquare`
 + `AttributeParen`
 + `BinaryOperator`
 + `BitFieldColon`
 + `BlockComment`
 + `CastRParen`
 + `ConditionalExpr`
 + `ConflictAlternative`
 + `ConflictEnd`
 + `ConflictStart`
 + `CtorInitializerColon`
 + `CtorInitializerComma`
 + `DesignatedInitializerLSquare`
 + `DesignatedInitializerPeriod`
 + `DictLiteral`
 + `ForEachMacro`
 + `FunctionAnnotationRParen`
 + `FunctionDeclarationName`
 + `FunctionLBrace`
 + `FunctionTypeLParen`
 + `ImplicitStringLiteral`
 + `InheritanceColon`
 + `InheritanceComma`
 + `InlineASMBrace`
 + `InlineASMColon`
 + `LineComment`
 + `MacroBlockBegin`
 + `MacroBlockEnd`
 + `OverloadedOperator`
 + `OverloadedOperatorLParen`
 + `PointerOrReference`
 + `RangeBasedForLoopColon`
 + `RegexLiteral`
 + `SelectorName`
 + `StartOfName`
 + `StructuredBindingLSquare`
 + `TemplateCloser`
 + `TemplateOpener`
 + `TemplateString`
 + `TrailingAnnotation`
 + `TrailingReturnArrow`
 + `TrailingUnaryOperator`
 + `UnaryOperator`
 + `Unknown`
