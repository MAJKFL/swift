// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation
import SwiftSyntax // expected-error {{unexpected error produced: no such module 'SwiftSyntax'}}

enum ActiveClauseEvaluator {
  case configuredRegions(ConfiguredRegions)
  case configuration(any BuildConfiguration)

  /// Previously-known diagnostics.
  var priorDiagnostics: [Diagnostic] {
    switch self {
    case .configuredRegions(let configuredRegions):
      return configuredRegions.diagnostics
    case .configuration:
      return []
    }
  }

  /// Determine which clause of an `#if` declaration is active, if any.
  ///
  /// If this evaluation produced any diagnostics, they will be appended to
  /// the diagnostics parameter.
  func activeClause(
    for node: IfConfigDeclSyntax,
    diagnostics: inout [Diagnostic]
  ) -> IfConfigClauseSyntax? {
    switch self {
    case .configuredRegions(let configuredRegions):
      return configuredRegions.activeClause(for: node)
    case .configuration(let configuration):
      let (activeClause, localDiagnostics) = node.activeClause(in: configuration)
      diagnostics.append(contentsOf: localDiagnostics)
      return activeClause
    }
  }
}

extension SyntaxProtocol {
  /// Produce a copy of this syntax node that removes all syntax regions that
  /// are inactive according to the given build configuration, leaving only
  /// the code that is active within that build configuration.
  ///
  /// If there are errors in the conditions of any configuration
  /// clauses, e.g., `#if FOO > 10`, then the condition will be
  /// considered to have failed and the clauses's elements will be
  /// removed.
  /// - Parameters:
  ///   - configuration: the configuration to apply.
  /// - Returns: the syntax node with all inactive regions removed, along with
  ///   an array containing any diagnostics produced along the way.
  public func removingInactive(
    in configuration: some BuildConfiguration
  ) -> (result: Syntax, diagnostics: [Diagnostic]) {
    return removingInactive(in: configuration, retainFeatureCheckIfConfigs: false)
  }

  /// Produce a copy of this syntax node that removes all syntax regions that
  /// are inactive according to the given build configuration, leaving only
  /// the code that is active within that build configuration.
  ///
  /// If there are errors in the conditions of any configuration
  /// clauses, e.g., `#if FOO > 10`, then the condition will be
  /// considered to have failed and the clauses's elements will be
  /// removed.
  /// - Parameters:
  ///   - configuration: the configuration to apply.
  ///   - retainFeatureCheckIfConfigs: whether to retain `#if` blocks involving
  ///     compiler version checks (e.g., `compiler(>=6.0)`) and `$`-based
  ///     feature checks.
  /// - Returns: the syntax node with all inactive regions removed, along with
  ///   an array containing any diagnostics produced along the way.
  @_spi(Compiler)
  public func removingInactive(
    in configuration: some BuildConfiguration,
    retainFeatureCheckIfConfigs: Bool
  ) -> (result: Syntax, diagnostics: [Diagnostic]) {
    // Rewrite the syntax tree by removing the inactive clauses
    // from each #if (along with the #ifs themselves).
    let rewriter = ActiveSyntaxRewriter(
      configuration: configuration,
      retainFeatureCheckIfConfigs: retainFeatureCheckIfConfigs
    )
    return (
      rewriter.rewrite(Syntax(self)),
      rewriter.diagnostics
    )
  }
}

extension ConfiguredRegions {
  /// Produce a copy of some syntax node in the configured region that removes
  /// all syntax regions that are inactive according to the build configuration,
  /// leaving only the code that is active within that build configuration.
  ///
  /// If there are errors in the conditions of any configuration
  /// clauses, e.g., `#if FOO > 10`, then the condition will be
  /// considered to have failed and the clauses's elements will be
  /// removed.
  /// - Parameters:
  ///   - node: the stnrax node from which inactive regions will be removed.
  /// - Returns: the syntax node with all inactive regions removed.
  public func removingInactive(from node: some SyntaxProtocol) -> Syntax {
    return removingInactive(from: node, retainFeatureCheckIfConfigs: false)
  }

  /// Produce a copy of some syntax node in the configured region that removes
  /// all syntax regions that are inactive according to the build configuration,
  /// leaving only the code that is active within that build configuration.
  ///
  /// If there are errors in the conditions of any configuration
  /// clauses, e.g., `#if FOO > 10`, then the condition will be
  /// considered to have failed and the clauses's elements will be
  /// removed.
  /// - Parameters:
  ///   - node: the stnrax node from which inactive regions will be removed.
  ///   - retainFeatureCheckIfConfigs: whether to retain `#if` blocks involving
  ///     compiler version checks (e.g., `compiler(>=6.0)`) and `$`-based
  ///     feature checks.
  /// - Returns: the syntax node with all inactive regions removed.
  @_spi(Compiler)
  public func removingInactive(
    from node: some SyntaxProtocol,
    retainFeatureCheckIfConfigs: Bool
  ) -> Syntax {
    // If there are no inactive regions, there's nothing to do.
    if regions.isEmpty {
      return Syntax(node)
    }

    // Rewrite the syntax tree by removing the inactive clauses
    // from each #if (along with the #ifs themselves).
    let rewriter = ActiveSyntaxRewriter(
      configuredRegions: self,
      retainFeatureCheckIfConfigs: retainFeatureCheckIfConfigs
    )
    return rewriter.rewrite(Syntax(node))
  }
}

/// Syntax rewriter that only visits syntax nodes that are active according
/// to a particular build configuration.
///
/// Given an example such as
///
/// ```swift
/// #if os(Linux)
/// func f() { }
/// #elseif os(iOS)
/// func g() { }
/// #endif
/// ```
///
/// the rewriter will eliminate nodes for inactive clauses, leaving only
/// those nodes that are in active clauses. When rewriting the above given
/// a build configuration for Linux, the resulting tree will be
///
/// ```swift
/// func f() { }
/// ```
///
/// When rewriting the above given a build configuration for iOS, the resulting
/// tree will be
///
/// ```swift
/// func g() { }
/// ```
///
/// For any other target platforms, the resulting tree will be empty (other
/// than trivia).
class ActiveSyntaxRewriter: SyntaxRewriter {
  let activeClauses: ActiveClauseEvaluator
  var diagnostics: [Diagnostic]

  /// Whether to retain `#if` blocks containing compiler and feature checks.
  var retainFeatureCheckIfConfigs: Bool

  init(configuredRegions: ConfiguredRegions, retainFeatureCheckIfConfigs: Bool) {
    self.activeClauses = .configuredRegions(configuredRegions)
    self.diagnostics = activeClauses.priorDiagnostics
    self.retainFeatureCheckIfConfigs = retainFeatureCheckIfConfigs
  }

  init(configuration: some BuildConfiguration, retainFeatureCheckIfConfigs: Bool) {
    self.activeClauses = .configuration(configuration)
    self.diagnostics = activeClauses.priorDiagnostics
    self.retainFeatureCheckIfConfigs = retainFeatureCheckIfConfigs
  }

  private func dropInactive<List: SyntaxCollection>(
    _ node: List,
    elementAsIfConfig: (List.Element) -> IfConfigDeclSyntax?
  ) -> List {
    var newElements: [List.Element] = []
    var anyChanged = false

    // Note that an element changed at the given index.
    func noteElementChanged(at elementIndex: List.Index) {
      if anyChanged {
        return
      }

      // This is the first element that changed, so note that we have
      // changes and add all prior elements to the list of new elements.
      anyChanged = true
      newElements.append(contentsOf: node[..<elementIndex])
    }

    for elementIndex in node.indices {
      let element = node[elementIndex]

      // Find #ifs within the list.
      if let ifConfigDecl = elementAsIfConfig(element),
        (!retainFeatureCheckIfConfigs || !ifConfigDecl.containsFeatureCheck)
      {
        // Retrieve the active `#if` clause
        let activeClause = activeClauses.activeClause(for: ifConfigDecl, diagnostics: &diagnostics)

        noteElementChanged(at: elementIndex)

        // Extract the elements from the active clause, if there are any.
        guard let elements = activeClause?.elements else {
          continue
        }

        // In a well-formed syntax tree, the element list is always the
        // same type as List. However, handle a manually-constructed,
        // ill-formed syntax tree gracefully by dropping the inner elements
        // as well.
        if let innerElements = Syntax(elements).as(List.self) {
          let newInnerElements = dropInactive(innerElements, elementAsIfConfig: elementAsIfConfig)
          newElements.append(contentsOf: newInnerElements)
        }

        continue
      }

      // Transform the element directly. If it changed, note the changes.
      if let transformedElement = rewrite(Syntax(element)).as(List.Element.self),
        transformedElement.id != element.id
      {
        noteElementChanged(at: elementIndex)
        newElements.append(transformedElement)
        continue
      }

      if anyChanged {
        newElements.append(element)
      }
    }

    if !anyChanged {
      return node
    }

    return List(newElements)
  }

  override func visit(_ node: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    return dropInactive(node) { element in
      guard case .decl(let declElement) = element.item else {
        return nil
      }

      return declElement.as(IfConfigDeclSyntax.self)
    }
  }

  override func visit(_ node: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
    return dropInactive(node) { element in
      return element.decl.as(IfConfigDeclSyntax.self)
    }
  }

  override func visit(_ node: SwitchCaseListSyntax) -> SwitchCaseListSyntax {
    return dropInactive(node) { element in
      if case .ifConfigDecl(let ifConfigDecl) = element {
        return ifConfigDecl
      }

      return nil
    }
  }

  override func visit(_ node: AttributeListSyntax) -> AttributeListSyntax {
    return dropInactive(node) { element in
      if case .ifConfigDecl(let ifConfigDecl) = element {
        return ifConfigDecl
      }

      return nil
    }
  }

  /// Apply the given base to the postfix expression.
  private func applyBaseToPostfixExpression(
    base: ExprSyntax,
    postfix: ExprSyntax
  ) -> ExprSyntax {
    /// Try to apply the base to the postfix expression using the given
    /// keypath into a specific node type.
    ///
    /// Returns the new expression node on success, `nil` when the node kind
    /// didn't match.
    func tryApply<Node: ExprSyntaxProtocol>(
      _ keyPath: WritableKeyPath<Node, ExprSyntax>
    ) -> ExprSyntax? {
      guard let node = postfix.as(Node.self) else {
        return nil
      }

      let newExpr = applyBaseToPostfixExpression(base: base, postfix: visit(node[keyPath: keyPath]))
      return ExprSyntax(node.with(keyPath, newExpr))
    }

    // Member access
    if let memberAccess = postfix.as(MemberAccessExprSyntax.self) {
      guard let memberBase = memberAccess.base else {
        // If this member access has no base, this is the base we are
        // replacing, terminating the recursion. Do so now.
        return ExprSyntax(memberAccess.with(\.base, visit(base)))
      }

      let newBase = applyBaseToPostfixExpression(base: base, postfix: memberBase)
      return ExprSyntax(memberAccess.with(\.base, newBase))
    }

    // Generic arguments <...>
    if let result = tryApply(\SpecializeExprSyntax.expression) {
      return result
    }

    // Call (...)
    if let result = tryApply(\FunctionCallExprSyntax.calledExpression) {
      return result
    }

    // Subscript [...]
    if let result = tryApply(\SubscriptExprSyntax.calledExpression) {
      return result
    }

    // Optional chaining ?
    if let result = tryApply(\OptionalChainingExprSyntax.expression) {
      return result
    }

    // Forced optional value !
    if let result = tryApply(\ForcedValueExprSyntax.expression) {
      return result
    }

    // Postfix unary operator.
    if let result = tryApply(\PostfixUnaryExprSyntax.expression) {
      return result
    }

    // #if
    if let postfixIfConfig = postfix.as(PostfixIfConfigExprSyntax.self) {
      return dropInactive(outerBase: base, postfixIfConfig: postfixIfConfig)
    }

    assertionFailure("Unhandled postfix expression in #if elimination")
    return postfix
  }

  /// Drop inactive regions from a postfix `#if` configuration, applying the
  /// outer "base" expression to the rewritten node.
  private func dropInactive(
    outerBase: ExprSyntax?,
    postfixIfConfig: PostfixIfConfigExprSyntax
  ) -> ExprSyntax {
    // If we're supposed to retain #if configs that are feature checks, and
    // this configuration has one, do so.
    if retainFeatureCheckIfConfigs && postfixIfConfig.config.containsFeatureCheck {
      return ExprSyntax(postfixIfConfig)
    }

    // Retrieve the active `if` clause.
    let activeClause = activeClauses.activeClause(for: postfixIfConfig.config, diagnostics: &diagnostics)

    guard case .postfixExpression(let postfixExpr) = activeClause?.elements
    else {
      // If there is no active clause, return the base.

      // Prefer the base we have and, if not, use the outer base. We can
      // only have both in an ill-formed syntax tree that was manually
      // created.
      if let base = postfixIfConfig.base ?? outerBase {
        return visit(base)
      }

      // If there was no base, we're in an erroneous syntax tree that would
      // never be produced by the parser. Synthesize a missing expression
      // syntax node so clients can recover more gracefully.
      return ExprSyntax(
        MissingExprSyntax(
          placeholder: .init(.identifier("<#expression#>"), presence: .missing)
        )
      )
    }

    // If there is no base, return the postfix expression.
    guard let base = postfixIfConfig.base ?? outerBase else {
      return visit(postfixExpr)
    }

    // Apply the base to the postfix expression.
    return applyBaseToPostfixExpression(base: base, postfix: postfixExpr)
  }

  override func visit(_ node: PostfixIfConfigExprSyntax) -> ExprSyntax {
    return dropInactive(outerBase: nil, postfixIfConfig: node)
  }
}

/// Helper class to find a feature or compiler check.
fileprivate class FindFeatureCheckVisitor: SyntaxVisitor {
  var foundFeatureCheck = false

  override func visit(_ node: DeclReferenceExprSyntax) -> SyntaxVisitorContinueKind {
    // Checks that start with $ are feature checks that should be retained.
    if let identifier = node.simpleIdentifier,
      let initialChar = identifier.name.first,
      initialChar == "$"
    {
      foundFeatureCheck = true
      return .skipChildren
    }

    return .visitChildren
  }

  override func visit(_ node: FunctionCallExprSyntax) -> SyntaxVisitorContinueKind {
    if let calleeDeclRef = node.calledExpression.as(DeclReferenceExprSyntax.self),
      let calleeName = calleeDeclRef.simpleIdentifier?.name,
      (calleeName == "compiler" || calleeName == "_compiler_version")
    {
      foundFeatureCheck = true
    }

    return .skipChildren
  }
}

extension ExprSyntaxProtocol {
  /// Whether any of the nodes in this expression involve compiler or feature
  /// checks.
  fileprivate var containsFeatureCheck: Bool {
    let visitor = FindFeatureCheckVisitor(viewMode: .fixedUp)
    visitor.walk(self)
    return visitor.foundFeatureCheck
  }
}

extension IfConfigDeclSyntax {
  /// Whether any of the clauses in this #if contain a feature check.
  var containsFeatureCheck: Bool {
    return clauses.contains { clause in
      if let condition = clause.condition {
        return condition.containsFeatureCheck
      } else {
        return false
      }
    }
  }
}

extension SyntaxProtocol {
  // Produce the source code for this syntax node with all of the comments
  // and #sourceLocations removed. Each comment will be replaced with either
  // a newline or a space, depending on whether the comment involved a newline.
  @_spi(Compiler)
  public var descriptionWithoutCommentsAndSourceLocations: String {
    var result = ""
    var skipUntilRParen = false
    for token in tokens(viewMode: .sourceAccurate) {
      // Skip #sourceLocation(...).
      if token.tokenKind == .poundSourceLocation {
        skipUntilRParen = true
        continue
      }

      if skipUntilRParen {
        if token.tokenKind == .rightParen {
          skipUntilRParen = false
        }
        continue
      }

      token.leadingTrivia.writeWithoutComments(to: &result)
      token.text.write(to: &result)
      token.trailingTrivia.writeWithoutComments(to: &result)
    }
    return result
  }
}

extension Trivia {
  fileprivate func writeWithoutComments(to stream: inout some TextOutputStream) {
    for piece in pieces {
      switch piece {
      case .backslashes, .carriageReturnLineFeeds, .carriageReturns, .formfeeds, .newlines, .pounds, .spaces, .tabs,
        .unexpectedText, .verticalTabs:
        piece.write(to: &stream)

      case .blockComment(let text), .docBlockComment(let text), .docLineComment(let text), .lineComment(let text):
        if text.contains(where: \.isNewline) {
          stream.write("\n")
        } else {
          stream.write(" ")
        }
      }
    }
  }
}

open class ActiveSyntaxVisitor: SyntaxVisitor {
  /// The abstracted build configuration, which will be queried for each
  /// relevant `#if`.
  let activeClauses: ActiveClauseEvaluator

  /// The diagnostics accumulated during this walk of active syntax.
  public private(set) var diagnostics: [Diagnostic] = []

  public init(viewMode: SyntaxTreeViewMode, configuration: some BuildConfiguration) {
    self.activeClauses = .configuration(configuration)
    self.diagnostics = activeClauses.priorDiagnostics
    super.init(viewMode: viewMode)
  }

  public init(viewMode: SyntaxTreeViewMode, configuredRegions: ConfiguredRegions) {
    self.activeClauses = .configuredRegions(configuredRegions)
    self.diagnostics = activeClauses.priorDiagnostics
    super.init(viewMode: viewMode)
  }

  open override func visit(_ node: IfConfigDeclSyntax) -> SyntaxVisitorContinueKind {
    // Note: there is a clone of this code in ActiveSyntaxAnyVisitor. If you
    // change one, please also change the other.
    let activeClause = activeClauses.activeClause(for: node, diagnostics: &diagnostics)

    // If there is an active clause, visit it's children.
    if let activeClause, let elements = activeClause.elements {
      walk(elements)
    }

    // Skip everything else in the #if.
    return .skipChildren
  }
}

/// A syntax visitor that only visits the syntax nodes that are active
/// according to a particular build configuration.
///
/// This subclass of `SyntaxVisitor` walks all of the syntax nodes in a given
/// tree that are "active" according to a particular build configuration,
/// meaning that the syntax would contribute to the resulting program when
/// when compiled with that configuration. For example, given:
///
/// ```
/// #if DEBUG
///   #if os(Linux)
///    func f()
///   #elseif os(iOS)
///    func g()
///   #endif
/// #endif
/// ```
///
/// And a build targeting Linux with the custom condition `DEBUG` set, a
/// complete walk via this visitor would visit `func f` but not `func g`. If
/// the build configuration instead targted macOS (but still had `DEBUG` set),
/// it would not visit either `f` or `g`.
///
/// All notes visited by this visitor will have the "active" state, i.e.,
/// `node.isActive(in: configuration)` will have evaluated to `.active`.
/// When errors occur, they will be recorded in the array of diagnostics.
open class ActiveSyntaxAnyVisitor: SyntaxAnyVisitor {
  /// The abstracted build configuration, which will be queried for each
  /// relevant `#if`.
  let activeClauses: ActiveClauseEvaluator

  /// The diagnostics accumulated during this walk of active syntax.
  public private(set) var diagnostics: [Diagnostic] = []

  public init(viewMode: SyntaxTreeViewMode, configuration: some BuildConfiguration) {
    self.activeClauses = .configuration(configuration)
    self.diagnostics = activeClauses.priorDiagnostics
    super.init(viewMode: viewMode)
  }

  public init(viewMode: SyntaxTreeViewMode, configuredRegions: ConfiguredRegions) {
    self.activeClauses = .configuredRegions(configuredRegions)
    self.diagnostics = activeClauses.priorDiagnostics
    super.init(viewMode: viewMode)
  }

  open override func visit(_ node: IfConfigDeclSyntax) -> SyntaxVisitorContinueKind {
    // Note: there is a clone of this code in ActiveSyntaxVisitor. If you
    // change one, please also change the other.

    // If there is an active clause, visit it's children.
    let activeClause = activeClauses.activeClause(for: node, diagnostics: &diagnostics)

    if let activeClause, let elements = activeClause.elements {
      walk(elements)
    }

    // Skip everything else in the #if.
    return .skipChildren
  }
}

public enum Endianness: String {
  /// Little endian, meaning that the least significant byte of a word is
  /// stored at the lowest address.
  case little

  /// Big endian, meaning that the most significant byte of a word is stored
  /// at the lowest address.
  case big
}

/// Describes the requested version of a module.
public enum CanImportVersion {
  /// Any version of the module will suffice.
  case unversioned

  /// Only modules with the given version or higher will match.
  case version(VersionTuple)

  /// Only modules where the underlying Clang module has the given version or
  /// higher will match.
  case underlyingVersion(VersionTuple)
}

/// Captures information about the build configuration that can be
/// queried in a `#if` expression, including OS, compiler version,
/// enabled language features, and available modules.
///
/// Providing complete build configuration information effectively requires
/// a Swift compiler, because (for example) determining whether a module can
/// be imported is a complicated task only implemented in the Swift compiler.
/// Therefore, queries are permitted to throw an error to report when they
/// cannot answer a query, in which case this error will be reported to
/// the caller and the condition will be treated as being "false", so the
/// code covered by the condition will be inactive.
public protocol BuildConfiguration {
  /// Determine whether a given custom build condition has been set.
  ///
  /// Custom build conditions can be set by the `-D` command line option to
  /// the Swift compiler. For example, `-DDEBUG` sets the custom condition
  /// named `DEBUG`, which could be checked with, e.g.,
  ///
  /// ```swift
  /// #if DEBUG
  /// // ...
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the custom build condition being checked (e.g.,
  ///     `DEBUG`.
  /// - Returns: Whether the custom condition is set.
  func isCustomConditionSet(name: String) throws -> Bool

  /// Determine whether the given feature is enabled.
  ///
  /// Features are determined by the Swift compiler, language mode, and other
  /// options such as `--enable-upcoming-feature`, and can be checked with
  /// the `hasFeature` syntax, e.g.,
  ///
  /// ```swift
  /// #if hasFeature(VariadicGenerics)
  /// // ...
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the feature being checked.
  /// - Returns: Whether the requested feature is available.
  func hasFeature(name: String) throws -> Bool

  /// Determine whether the given attribute is available.
  ///
  /// Attributes are determined by the Swift compiler. They can be checked
  /// with `hasAttribute` syntax, e.g.,
  ///
  /// ```swift
  /// #if hasAttribute(available)
  /// // ...
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the attribute being queried.
  /// - Returns: Whether the requested attribute is supported.
  func hasAttribute(name: String) throws -> Bool

  /// Determine whether a module with the given import path can be imported,
  /// with additional version information.
  ///
  /// The availability of a module for import can be checked with `canImport`,
  /// e.g.,
  ///
  /// ```swift
  /// #if canImport(UIKit)
  /// // ...
  /// #endif
  /// ```
  ///
  /// There is an experimental syntax for providing required module version
  /// information, which will translate into the `version` argument.
  ///
  /// - Parameters:
  ///   - importPath: A nonempty sequence of (token, identifier) pairs
  ///     describing the imported module, which was written in source as a
  ///     dotted sequence, e.g., `UIKit.UIViewController` will be passed in as
  ///     the import path array `[(token, "UIKit"), (token, "UIViewController")]`.
  ///   - version: The version restriction on the imported module. For the
  ///     normal `canImport(<import-path>)` syntax, this will always be
  ///     `CanImportVersion.unversioned`.
  /// - Returns: Whether the module can be imported.
  func canImport(importPath: [(TokenSyntax, String)], version: CanImportVersion) throws -> Bool

  /// Determine whether the given name is the active target OS (e.g., Linux, iOS).
  ///
  /// The target operating system can be queried with `os(<name>)`, e.g.,
  ///
  /// ```swift
  /// #if os(Linux)
  /// // Linux-specific implementation
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the operating system being queried, such as `Linux`,
  ///   `Windows`, `macOS`, etc.
  /// - Returns: Whether the given operating system name is the target operating
  ///   system, i.e., the operating system for which code is being generated.
  func isActiveTargetOS(name: String) throws -> Bool

  /// Determine whether the given name is the active target architecture
  /// (e.g., x86_64, arm64).
  ///
  /// The target processor architecture can be queried with `arch(<name>)`, e.g.,
  ///
  /// ```swift
  /// #if arch(x86_64)
  /// // 64-bit x86 Intel-specific code
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the target architecture to check.
  /// - Returns: Whether the given processor architecture is the target
  ///   architecture.
  func isActiveTargetArchitecture(name: String) throws -> Bool

  /// Determine whether the given name is the active target environment (e.g., simulator)
  ///
  /// The target environment can be queried with `targetEnvironment(<name>)`,
  /// e.g.,
  ///
  /// ```swift
  /// #if targetEnvironment(simulator)
  /// // Simulator-specific code
  /// #endif
  /// ```
  ///
  /// - Parameters:
  ///   - name: The name of the target environment to check.
  /// - Returns: Whether the target platform is for a specific environment,
  ///   such as a simulator or emulator.
  func isActiveTargetEnvironment(name: String) throws -> Bool

  /// Determine whether the given name is the active target runtime (e.g., _ObjC vs. _Native)
  ///
  /// The target runtime can only be queried by an experimental syntax
  /// `_runtime(<name>)`, e.g.,
  ///
  /// ```swift
  /// #if _runtime(_ObjC)
  /// // Code that depends on Swift being built for use with the Objective-C
  /// // runtime, e.g., on Apple platforms.
  /// #endif
  /// ```
  ///
  /// The only other runtime is "none", when Swift isn't tying into any other
  /// specific runtime.
  ///
  /// - Parameters:
  ///   - name: The name of the runtime.
  /// - Returns: Whether the target runtime matches the given name.
  func isActiveTargetRuntime(name: String) throws -> Bool

  /// Determine whether the given name is the active target pointer authentication scheme (e.g., arm64e).
  ///
  /// The target pointer authentication scheme describes how pointers are
  /// signed, as a security mitigation. This scheme can only be queried by
  /// an experimental syntax `_ptrath(<name>)`, e.g.,
  ///
  /// ```swift
  /// #if _ptrauth(arm64e)
  /// // Special logic for arm64e pointer signing
  /// #endif
  /// ```
  /// - Parameters:
  ///   - name: The name of the pointer authentication scheme to check.
  /// - Returns: Whether the code generated for the target will use the given
  /// pointer authentication scheme.
  func isActiveTargetPointerAuthentication(name: String) throws -> Bool

  /// The bit width of a data pointer for the target architecture.
  ///
  /// The target's pointer bit width (which also corresponds to the number of
  /// bits in `Int`/`UInt`) can only be queried with the experimental syntax
  /// `_pointerBitWidth(_<bitwidth>)`, e.g.,
  ///
  /// ```swift
  /// #if _pointerBitWidth(_32)
  /// // 32-bit system
  /// #endif
  /// ```
  var targetPointerBitWidth: Int { get }

  /// The atomic bit widths that are natively supported by the target
  /// architecture.
  ///
  /// This lists all of the bit widths for which the target provides support
  /// for atomic operations. It can be queried with
  /// `_hasAtomicBitWidth(_<bitwidth>)`, e.g.
  ///
  /// ```swift
  /// #if _hasAtomicBitWidth(_64)
  /// // 64-bit atomics are available
  /// #endif
  var targetAtomicBitWidths: [Int] { get }

  /// The endianness of the target architecture.
  ///
  /// The target's endianness can onyl be queried with the experimental syntax
  /// `_endian(<name>)`, where `<name>` can be either "big" or "little", e.g.,
  ///
  /// ```swift
  /// #if _endian(little)
  /// // Swap some bytes around for network byte order
  /// #endif
  /// ```
  var endianness: Endianness { get }

  /// The effective language version, which can be set by the user (e.g., 5.0).
  ///
  /// The language version can be queried with the `swift` directive that checks
  /// how the supported language version compares, as described by
  /// [SE-0212](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0212-compiler-version-directive.md). For example:
  ///
  /// ```swift
  /// #if swift(>=5.5)
  /// // Hooray, we can use tasks!
  /// ```
  var languageVersion: VersionTuple { get }

  /// The version of the compiler (e.g., 5.9).
  ///
  /// The compiler version can be queried with the `compiler` directive that
  /// checks the specific version of the compiler being used to process the
  /// code, e.g.,
  ///
  /// ```swift
  /// #if compiler(>=5.7)
  /// // Hoorway, we can implicitly open existentials!
  /// #endif
  var compilerVersion: VersionTuple { get }
}

public struct ConfiguredRegions {
  let regions: [(ifClause: IfConfigClauseSyntax, state: IfConfigRegionState)]

  // A mapping from each of the #if declarations that have been evaluated to
  // the active clause. Absence from this map means that there is no active
  // clause, either because every clause failed or because the entire #if
  // itself is inactive.
  var activeClauses: [IfConfigDeclSyntax: IfConfigClauseSyntax]

  /// The set of diagnostics produced when evaluating the configured regions.
  public let diagnostics: [Diagnostic]

  /// Determine whether the given syntax node is active within the configured
  /// regions.
  ///
  /// Any given node within the range of configured regions can either be
  /// "active" (it is part of the program), "inactive" (it is not part of the
  /// program), or "unparsed" (it is not part of the program and shouldn't
  /// produce any syntax errors).
  ///
  /// This operation takes time that is logarthmic in the number of regions
  /// in the syntax tree.
  public func isActive(_ node: some SyntaxProtocol) -> IfConfigRegionState {
    // Find the slice of the regions in which this node lands.
    var currentSlice = regions[...]
    while !currentSlice.isEmpty {
      let middle = currentSlice.startIndex + currentSlice.count / 2

      // If the node is prior to the start of the middle, take the left-hand side.
      if node.position < currentSlice[middle].ifClause.regionStart {
        currentSlice = currentSlice[..<middle]
        continue
      }

      // If the node is after the end of the middle, take the right-hand side.
      if node.position > currentSlice[middle].ifClause.endPosition {
        currentSlice = currentSlice[(middle + 1)...]
        continue
      }

      // We cannot narrow the range any further.
      break
    }

    // Find the last region in which this node lands. If there is no such
    // region, this is active.
    return currentSlice.last { region in
      (region.ifClause.regionStart...region.ifClause.endPosition).contains(node.position)
    }?.state ?? .active
  }

  /// Determine which clause of an `#if` declaration was active within this
  /// set of configured regions.
  ///
  /// A particular `#if` declaration might have no active clause (e.g., this
  /// operation will return a `nil`) if either none of the clauses had
  /// conditions that succeeded, or the `#if` declaration itself is within an
  /// inactive (or unparsed) region and therefore cannot have an active clause.
  public func activeClause(for node: IfConfigDeclSyntax) -> IfConfigClauseSyntax? {
    return activeClauses[node]
  }
}

extension ConfiguredRegions: RandomAccessCollection {
  public typealias Element = (ifClause: IfConfigClauseSyntax, state: IfConfigRegionState)
  public var startIndex: Int { regions.startIndex }
  public var endIndex: Int { regions.endIndex }

  public subscript(index: Int) -> Element {
    regions[index]
  }
}

extension ConfiguredRegions: CustomDebugStringConvertible {
  /// Provides source ranges for each of the configured regions.
  public var debugDescription: String {
    guard let firstRegion = first else {
      return "[]"
    }

    let root = firstRegion.ifClause.root
    let converter = SourceLocationConverter(fileName: "", tree: root)
    let regionDescriptions = regions.map { (ifClause, state) in
      let startPosition = converter.location(for: ifClause.position)
      let endPosition = converter.location(for: ifClause.endPosition)
      return "[\(startPosition.line):\(startPosition.column) - \(endPosition.line):\(endPosition.column)] = \(state)"
    }

    return "[\(regionDescriptions.joined(separator: ", ")))]"
  }
}

extension IfConfigClauseSyntax {
  /// The effective start of the region after which code is subject to its
  /// condition.
  fileprivate var regionStart: AbsolutePosition {
    condition?.endPosition ?? elements?._syntaxNode.position ?? poundKeyword.endPosition
  }
}

extension SyntaxProtocol {
  /// Find all of the #if/#elseif/#else clauses within the given syntax node,
  /// indicating their active state. This operation will recurse into all
  /// clauses to indicate regions of active / inactive / unparsed code.
  ///
  /// For example, given code like the following:
  /// #if DEBUG
  ///   #if A
  ///     func f()
  ///   #elseif B
  ///     func g()
  ///   #elseif compiler(>= 12.0)
  ///   please print the number after 41
  ///   #endif
  /// #else
  /// #endif
  ///
  /// If the configuration options `DEBUG` and `B` are provided, but `A` is not,
  /// and the compiler version is less than 12.0, the results will be contain:
  ///   - Active region for the `#if DEBUG`.
  ///   - Inactive region for the `#if A`.
  ///   - Active region for the `#elseif B`.
  ///   - Unparsed region for the `#elseif compiler(>= 12.0)`.
  ///   - Inactive region for the final `#else`.
  public func configuredRegions(
    in configuration: some BuildConfiguration
  ) -> ConfiguredRegions {
    let visitor = ConfiguredRegionVisitor(configuration: configuration)
    visitor.walk(self)
    return ConfiguredRegions(
      regions: visitor.regions,
      activeClauses: visitor.activeClauses,
      diagnostics: visitor.diagnostics
    )
  }
}

/// Helper class that walks a syntax tree looking for configured regions.
fileprivate class ConfiguredRegionVisitor<Configuration: BuildConfiguration>: SyntaxVisitor {
  let configuration: Configuration

  /// The regions we've found so far.
  var regions: [(IfConfigClauseSyntax, IfConfigRegionState)] = []

  /// Whether we are currently within an active region.
  var inActiveRegion = true

  /// Whether we are currently within an #if at all.
  var inAnyIfConfig = false

  // All diagnostics encountered along the way.
  var diagnostics: [Diagnostic] = []

  // A mapping from each of the #if declarations that have been evaluated to
  // the active clause. Absence from this map means that there is no active
  // clause, either because every clause failed or because the entire #if
  // itself is inactive.
  var activeClauses: [IfConfigDeclSyntax: IfConfigClauseSyntax] = [:]

  init(configuration: Configuration) {
    self.configuration = configuration
    super.init(viewMode: .sourceAccurate)
  }

  override func visit(_ node: IfConfigDeclSyntax) -> SyntaxVisitorContinueKind {
    // We are in an #if.
    let priorInAnyIfConfig = inAnyIfConfig
    inAnyIfConfig = true
    defer {
      inAnyIfConfig = priorInAnyIfConfig
    }

    // Walk through the clauses to find the active one.
    var foundActive = false
    var syntaxErrorsAllowed = false
    let outerState: IfConfigRegionState = inActiveRegion ? .active : .inactive
    for clause in node.clauses {
      let isActive: Bool
      if let condition = clause.condition {
        if !foundActive {
          // Fold operators so we can evaluate this #if condition.
          let (foldedCondition, foldDiagnostics) = IfConfigClauseSyntax.foldOperators(condition)
          diagnostics.append(contentsOf: foldDiagnostics)

          // In an active region, evaluate the condition to determine whether
          // this clause is active. Otherwise, this clause is inactive.
          if inActiveRegion {
            let (thisIsActive, _, evalDiagnostics) = evaluateIfConfig(
              condition: foldedCondition,
              configuration: configuration
            )
            diagnostics.append(contentsOf: evalDiagnostics)

            // Determine if there was an error that prevented us from
            // evaluating the condition. If so, we'll allow syntax errors
            // from here on out.
            let hadError =
              foldDiagnostics.contains { diag in
                diag.diagMessage.severity == .error
              }
              || evalDiagnostics.contains { diag in
                diag.diagMessage.severity == .error
              }

            if hadError {
              isActive = false
              syntaxErrorsAllowed = true
            } else {
              isActive = thisIsActive

              // Determine whether syntax errors are allowed.
              syntaxErrorsAllowed = foldedCondition.allowsSyntaxErrorsFolded
            }
          } else {
            isActive = false

            // Determine whether syntax errors are allowed, even though we
            // skipped evaluation of the actual condition.
            syntaxErrorsAllowed = foldedCondition.allowsSyntaxErrorsFolded
          }
        } else {
          // We already found an active condition, so this is inactive.
          isActive = false
        }
      } else {
        // This is an #else. It's active if we haven't found an active clause
        // yet and are in an active region.
        isActive = !foundActive && inActiveRegion
      }

      // If this is the active clause, record it as such.
      if isActive {
        activeClauses[node] = clause
      }

      // Determine and record the current state.
      let currentState: IfConfigRegionState
      switch (isActive, syntaxErrorsAllowed) {
      case (true, _): currentState = .active
      case (false, false): currentState = .inactive
      case (false, true): currentState = .unparsed
      }

      // If there is a state change, record it.
      if !priorInAnyIfConfig || currentState != .inactive || currentState != outerState {
        regions.append((clause, currentState))
      }

      // If this is a parsed region, recurse into it.
      if currentState != .unparsed, let elements = clause.elements {
        let priorInActiveRegion = inActiveRegion
        inActiveRegion = isActive
        defer {
          inActiveRegion = priorInActiveRegion
        }
        walk(elements)
      }

      // Note when we found an active clause.
      if isActive {
        foundActive = true
      }
    }

    return .skipChildren
  }
}

extension IfConfigDeclSyntax {
  /// Given a particular build configuration, determine which clause (if any) is the "active" clause.
  ///
  /// For example, for code like the following:
  /// ```
  /// #if A
  ///  func f()
  /// #elseif B
  ///  func g()
  /// #endif
  /// ```
  ///
  /// If the `A` configuration option was passed on the command line (e.g. via `-DA`), the first clause
  /// (containing `func f()`) would be returned. If not, and if the `B` configuration was passed on the
  /// command line, the second clause (containing `func g()`) would be returned. If neither was
  /// passed, this function will return `nil` to indicate that none of the regions are active.
  ///
  /// If an error occurs while processing any of the `#if` clauses,
  /// that clause will be considered inactive and this operation will
  /// continue to evaluate later clauses.
  public func activeClause(
    in configuration: some BuildConfiguration
  ) -> (clause: IfConfigClauseSyntax?, diagnostics: [Diagnostic]) {
    var diagnostics: [Diagnostic] = []
    for clause in clauses {
      // If there is no condition, we have reached an unconditional clause. Return it.
      guard let condition = clause.condition else {
        return (clause, diagnostics: diagnostics)
      }

      // Apply operator folding for !/&&/||.
      let (foldedCondition, foldingDiagnostics) = IfConfigClauseSyntax.foldOperators(condition)
      diagnostics.append(contentsOf: foldingDiagnostics)

      // If this condition evaluates true, return this clause.
      let (isActive, _, localDiagnostics) = evaluateIfConfig(
        condition: foldedCondition,
        configuration: configuration
      )
      diagnostics.append(contentsOf: localDiagnostics)

      if isActive {
        return (clause, diagnostics: diagnostics)
      }
    }

    return (nil, diagnostics: diagnostics)
  }
}

enum IfConfigDiagnostic: Error, CustomStringConvertible {
  case unknownExpression(ExprSyntax)
  case unhandledFunction(name: String, syntax: ExprSyntax)
  case requiresUnlabeledArgument(name: String, role: String, syntax: ExprSyntax)
  case unsupportedVersionOperator(name: String, operator: TokenSyntax)
  case invalidVersionOperand(name: String, syntax: ExprSyntax)
  case emptyVersionComponent(syntax: ExprSyntax)
  case compilerVersionOutOfRange(value: Int, upperLimit: Int, syntax: ExprSyntax)
  case compilerVersionSecondComponentNotWildcard(syntax: ExprSyntax)
  case compilerVersionTooManyComponents(syntax: ExprSyntax)
  case canImportMissingModule(syntax: ExprSyntax)
  case canImportLabel(syntax: ExprSyntax)
  case canImportTwoParameters(syntax: ExprSyntax)
  case ignoredTrailingComponents(version: VersionTuple, syntax: ExprSyntax)
  case integerLiteralCondition(syntax: ExprSyntax, replacement: Bool)
  case likelySimulatorPlatform(syntax: ExprSyntax)
  case likelyTargetOS(syntax: ExprSyntax, replacement: ExprSyntax?)
  case endiannessDoesNotMatch(syntax: ExprSyntax, argument: String)
  case macabiIsMacCatalyst(syntax: ExprSyntax)
  case expectedModuleName(syntax: ExprSyntax)
  case badInfixOperator(syntax: ExprSyntax)
  case badPrefixOperator(syntax: ExprSyntax)
  case unexpectedDefined(syntax: ExprSyntax, argument: String)

  var description: String {
    switch self {
    case .unknownExpression:
      return "invalid conditional compilation expression"

    case .unhandledFunction(name: let name, syntax: _):
      return "build configuration cannot handle '\(name)'"

    case .requiresUnlabeledArgument(name: let name, role: let role, syntax: _):
      return "'\(name)' requires a single unlabeled argument for the \(role)"

    case .unsupportedVersionOperator(name: let name, operator: let op):
      return "'\(name)' version check does not support operator '\(op.trimmedDescription)'"

    case .invalidVersionOperand(name: let name, syntax: let version):
      return "'\(name)' version check has invalid version '\(version.trimmedDescription)'"

    case .emptyVersionComponent(syntax: _):
      return "found empty version component"

    case .compilerVersionOutOfRange(value: let value, upperLimit: let upperLimit, syntax: _):
      return "compiler version component '\(value)' is not in the allowed range 0...\(upperLimit)"

    case .compilerVersionSecondComponentNotWildcard(syntax: _):
      return "the second version component is not used for comparison in legacy compiler versions"

    case .compilerVersionTooManyComponents(syntax: _):
      return "compiler version must not have more than five components"

    case .canImportMissingModule(syntax: _):
      return "'canImport' requires a module name"

    case .canImportLabel(syntax: _):
      return "second parameter of 'canImport' should be labeled as _version or _underlyingVersion"

    case .canImportTwoParameters(syntax: _):
      return "'canImport' can take only two parameters"

    case .ignoredTrailingComponents(version: let version, syntax: _):
      return "trailing components of version '\(version.description)' are ignored"

    case .integerLiteralCondition(syntax: let syntax, replacement: let replacement):
      return "'\(syntax.trimmedDescription)' is not a valid conditional compilation expression, use '\(replacement)'"

    case .likelySimulatorPlatform:
      return
        "platform condition appears to be testing for simulator environment; use 'targetEnvironment(simulator)' instead"

    case .likelyTargetOS(syntax: _, replacement: let replacement?):
      return "'TARGET_OS_*' preprocessor macros are not available in Swift; use '\(replacement)' instead"

    case .likelyTargetOS(syntax: _, replacement: nil):
      return "'TARGET_OS_*' preprocessor macros are not available in Swift; use 'os(...)' conditionals instead"

    case .macabiIsMacCatalyst:
      return "'macabi' has been renamed to 'macCatalyst'"

    case .endiannessDoesNotMatch:
      return "unknown endianness for build configuration '_endian' (must be 'big' or 'little')"

    case .expectedModuleName:
      return "expected module name"

    case .badInfixOperator:
      return "expected '&&' or '||' expression"

    case .badPrefixOperator:
      return "expected unary '!' expression"

    case .unexpectedDefined:
      return
        "compilation conditions in Swift are always boolean and do not need to be checked for existence with 'defined()'"
    }
  }

  /// Retrieve the syntax node associated with this error.
  var syntax: Syntax {
    switch self {
    case .unknownExpression(let syntax),
      .unhandledFunction(name: _, syntax: let syntax),
      .requiresUnlabeledArgument(name: _, role: _, syntax: let syntax),
      .invalidVersionOperand(name: _, syntax: let syntax),
      .emptyVersionComponent(syntax: let syntax),
      .compilerVersionOutOfRange(value: _, upperLimit: _, syntax: let syntax),
      .compilerVersionTooManyComponents(syntax: let syntax),
      .compilerVersionSecondComponentNotWildcard(syntax: let syntax),
      .canImportMissingModule(syntax: let syntax),
      .canImportLabel(syntax: let syntax),
      .canImportTwoParameters(syntax: let syntax),
      .ignoredTrailingComponents(version: _, syntax: let syntax),
      .integerLiteralCondition(syntax: let syntax, replacement: _),
      .likelySimulatorPlatform(syntax: let syntax),
      .likelyTargetOS(syntax: let syntax, replacement: _),
      .endiannessDoesNotMatch(syntax: let syntax, argument: _),
      .macabiIsMacCatalyst(syntax: let syntax),
      .expectedModuleName(syntax: let syntax),
      .badInfixOperator(syntax: let syntax),
      .badPrefixOperator(syntax: let syntax),
      .unexpectedDefined(syntax: let syntax, argument: _):
      return Syntax(syntax)

    case .unsupportedVersionOperator(name: _, operator: let op):
      return Syntax(op)
    }
  }
}

extension IfConfigDiagnostic: DiagnosticMessage {
  var message: String { description }

  var diagnosticID: MessageID {
    .init(domain: "SwiftIfConfig", id: "IfConfigDiagnostic")
  }

  var severity: SwiftDiagnostics.DiagnosticSeverity {
    switch self {
    case .compilerVersionSecondComponentNotWildcard, .ignoredTrailingComponents,
      .likelySimulatorPlatform, .likelyTargetOS, .endiannessDoesNotMatch, .macabiIsMacCatalyst:
      return .warning
    default: return .error
    }
  }

  private struct SimpleFixItMessage: FixItMessage {
    var message: String

    var fixItID: MessageID {
      .init(domain: "SwiftIfConfig", id: "IfConfigFixIt")
    }
  }

  var asDiagnostic: Diagnostic {
    // For the integer literal condition we have a Fix-It.
    if case .integerLiteralCondition(let syntax, let replacement) = self {
      return Diagnostic(
        node: syntax,
        message: self,
        fixIt: .replace(
          message: SimpleFixItMessage(
            message: "replace with Boolean literal '\(replacement)'"
          ),
          oldNode: syntax,
          newNode: BooleanLiteralExprSyntax(
            literal: .keyword(replacement ? .true : .false)
          )
        )
      )
    }

    // For the likely targetEnvironment(simulator) condition we have a Fix-It.
    if case .likelySimulatorPlatform(let syntax) = self {
      return Diagnostic(
        node: syntax,
        message: self,
        fixIt: .replace(
          message: SimpleFixItMessage(
            message: "replace with 'targetEnvironment(simulator)'"
          ),
          oldNode: syntax,
          newNode: "targetEnvironment(simulator)" as ExprSyntax
        )
      )
    }

    // For the likely TARGET_OS_* condition we may have a Fix-It.
    if case .likelyTargetOS(let syntax, let replacement?) = self {
      return Diagnostic(
        node: syntax,
        message: self,
        fixIt: .replace(
          message: SimpleFixItMessage(
            message: "replace with '\(replacement)'"
          ),
          oldNode: syntax,
          newNode: replacement
        )
      )
    }

    // For the targetEnvironment(macabi) -> macCatalyst rename we have a Fix-It.
    if case .macabiIsMacCatalyst(syntax: let syntax) = self {
      return Diagnostic(
        node: syntax,
        message: self,
        fixIt: .replace(
          message: SimpleFixItMessage(
            message: "replace with 'macCatalyst'"
          ),
          oldNode: syntax,
          newNode: "macCatalyst" as ExprSyntax
        )
      )
    }

    // For the targetEnvironment(macabi) -> macCatalyst rename we have a Fix-It.
    if case .unexpectedDefined(syntax: let syntax, argument: let argument) = self {
      return Diagnostic(
        node: syntax,
        message: self,
        fixIt: .replace(
          message: SimpleFixItMessage(
            message: "remove 'defined()'"
          ),
          oldNode: syntax,
          newNode: TokenSyntax.identifier(argument)
        )
      )
    }

    return Diagnostic(node: syntax, message: self)
  }
}

func evaluateIfConfig(
  condition: ExprSyntax,
  configuration: some BuildConfiguration
) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
  var extraDiagnostics: [Diagnostic] = []

  /// Record the error before returning the given value.
  func recordError(
    _ error: any Error,
    at node: some SyntaxProtocol
  ) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
    return (
      active: false,
      syntaxErrorsAllowed: true,
      diagnostics: extraDiagnostics + error.asDiagnostics(at: node)
    )
  }

  /// Record an if-config evaluation error before returning it. Use this for
  /// every 'throw' site in this evaluation.
  func recordError(
    _ error: IfConfigDiagnostic
  ) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
    return recordError(error, at: error.syntax)
  }

  /// Check a configuration condition, translating any thrown error into an
  /// appropriate diagnostic for the handler before rethrowing it.
  func checkConfiguration(
    at node: some SyntaxProtocol,
    body: () throws -> (Bool, Bool)
  ) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
    do {
      let (active, syntaxErrorsAllowed) = try body()
      return (active, syntaxErrorsAllowed, extraDiagnostics)
    } catch let error {
      return recordError(error, at: node)
    }
  }

  // Boolean literals evaluate as-is
  if let boolLiteral = condition.as(BooleanLiteralExprSyntax.self) {
    return (
      active: boolLiteral.literalValue,
      syntaxErrorsAllowed: false,
      diagnostics: extraDiagnostics
    )
  }

  // Integer literals aren't allowed, but we recognize them.
  if let intLiteral = condition.as(IntegerLiteralExprSyntax.self),
    (intLiteral.literal.text == "0" || intLiteral.literal.text == "1")
  {
    let result = intLiteral.literal.text == "1"

    return (
      active: result,
      syntaxErrorsAllowed: false,
      diagnostics: [
        IfConfigDiagnostic.integerLiteralCondition(
          syntax: condition,
          replacement: result
        ).asDiagnostic
      ]
    )
  }

  // Declaration references are for custom compilation flags.
  if let identExpr = condition.as(DeclReferenceExprSyntax.self),
    let ident = identExpr.simpleIdentifier?.name
  {
    if let targetOSDiagnostic = diagnoseLikelyTargetOSTest(at: identExpr, name: ident) {
      extraDiagnostics.append(targetOSDiagnostic)
    }
    // Evaluate the custom condition. If the build configuration cannot answer this query, fail.
    return checkConfiguration(at: identExpr) {
      (active: try configuration.isCustomConditionSet(name: ident), syntaxErrorsAllowed: false)
    }
  }

  // Logical '!'.
  if let prefixOp = condition.as(PrefixOperatorExprSyntax.self) {
    // If this isn't '!', complain.
    guard prefixOp.operator.text == "!" else {
      return recordError(.badPrefixOperator(syntax: condition))
    }

    let (innerActive, innerSyntaxErrorsAllowed, innerDiagnostics) = evaluateIfConfig(
      condition: prefixOp.expression,
      configuration: configuration
    )

    return (active: !innerActive, syntaxErrorsAllowed: innerSyntaxErrorsAllowed, diagnostics: innerDiagnostics)
  }

  // Logical '&&' and '||'.
  if let binOp = condition.as(InfixOperatorExprSyntax.self),
    let op = binOp.operator.as(BinaryOperatorExprSyntax.self)
  {
    // If this is neither && nor ||, it was already diagnosed as part of
    // operator folding. Just return this as inactive.
    guard op.operator.text == "&&" || op.operator.text == "||" else {
      return (active: false, syntaxErrorsAllowed: true, diagnostics: extraDiagnostics)
    }

    // Check whether this was likely to be a check for targetEnvironment(simulator).
    if binOp.isOutermostIfCondition,
      let targetEnvironmentDiag = diagnoseLikelySimulatorEnvironmentTest(binOp)
    {
      extraDiagnostics.append(targetEnvironmentDiag)
    }

    // Evaluate the left-hand side.
    let (lhsActive, lhsSyntaxErrorsAllowed, lhsDiagnostics) = evaluateIfConfig(
      condition: binOp.leftOperand,
      configuration: configuration
    )

    // Determine whether we already know the result. We might short-circuit the
    // evaluation, depending on whether we need to produce validation
    // diagnostics for the right-hand side.
    let shortCircuitResult: Bool?
    switch (lhsActive, op.operator.text) {
    case (true, "||"): shortCircuitResult = true
    case (false, "&&"): shortCircuitResult = false
    default: shortCircuitResult = nil
    }

    // If we are supposed to short-circuit and the left-hand side of this
    // operator permits syntax errors when it fails, stop now: we shouldn't
    // process the right-hand side at all.
    if let isActive = shortCircuitResult, lhsSyntaxErrorsAllowed {
      return (
        active: isActive,
        syntaxErrorsAllowed: lhsSyntaxErrorsAllowed,
        diagnostics: extraDiagnostics + lhsDiagnostics
      )
    }

    // Process the right-hand side. If we already know the answer, then
    // avoid performing any build configuration queries that might cause
    // side effects.
    let rhsActive: Bool
    let rhsSyntaxErrorsAllowed: Bool
    let rhsDiagnostics: [Diagnostic]
    if shortCircuitResult != nil {
      (rhsActive, rhsSyntaxErrorsAllowed, rhsDiagnostics) = evaluateIfConfig(
        condition: binOp.rightOperand,
        configuration: CanImportSuppressingBuildConfiguration(other: configuration)
      )
    } else {
      (rhsActive, rhsSyntaxErrorsAllowed, rhsDiagnostics) = evaluateIfConfig(
        condition: binOp.rightOperand,
        configuration: configuration
      )
    }

    switch op.operator.text {
    case "||":
      return (
        active: lhsActive || rhsActive,
        syntaxErrorsAllowed: lhsSyntaxErrorsAllowed && rhsSyntaxErrorsAllowed,
        diagnostics: extraDiagnostics + lhsDiagnostics + rhsDiagnostics
      )

    case "&&":
      return (
        active: lhsActive && rhsActive,
        syntaxErrorsAllowed: lhsSyntaxErrorsAllowed || rhsSyntaxErrorsAllowed,
        diagnostics: extraDiagnostics + lhsDiagnostics + rhsDiagnostics
      )

    default:
      fatalError("prevented by condition for getting here")
    }
  }

  // Look through parentheses.
  if let tuple = condition.as(TupleExprSyntax.self), tuple.isParentheses,
    let element = tuple.elements.first
  {
    return evaluateIfConfig(
      condition: element.expression,
      configuration: configuration
    )
  }

  // Call syntax is for operations.
  if let call = condition.as(FunctionCallExprSyntax.self),
    let fnName = call.calledExpression.simpleIdentifierExpr?.name,
    let fn = IfConfigFunctions(rawValue: fnName)
  {
    /// Perform a check for an operation that takes a single identifier argument.
    func doSingleIdentifierArgumentCheck(
      _ body: (String) throws -> Bool,
      role: String
    ) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
      // Ensure that we have a single argument that is a simple identifier.
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        var arg = argExpr.simpleIdentifierExpr?.name
      else {
        return recordError(
          .requiresUnlabeledArgument(name: fnName, role: role, syntax: ExprSyntax(call))
        )
      }

      // The historical "macabi" environment has been renamed to "macCatalyst".
      if role == "environment" && arg == "macabi" {
        extraDiagnostics.append(
          IfConfigDiagnostic.macabiIsMacCatalyst(syntax: argExpr)
            .asDiagnostic
        )

        arg = "macCatalyst"
      }

      return checkConfiguration(at: argExpr) {
        (active: try body(arg), syntaxErrorsAllowed: fn.syntaxErrorsAllowed)
      }
    }

    /// Perform a check for a version constraint as used in the "swift" or "compiler" version checks.
    func doVersionComparisonCheck(
      _ actualVersion: VersionTuple
    ) -> (active: Bool, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
      // Ensure that we have a single unlabeled argument that is either >= or < as a prefix
      // operator applied to a version.
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        let unaryArg = argExpr.as(PrefixOperatorExprSyntax.self)
      else {
        return recordError(
          .requiresUnlabeledArgument(
            name: fnName,
            role: "version comparison (>= or <= a version)",
            syntax: ExprSyntax(call)
          )
        )
      }

      // Parse the version.
      let opToken = unaryArg.operator
      guard let version = VersionTuple(parsing: unaryArg.expression.trimmedDescription) else {
        return recordError(.invalidVersionOperand(name: fnName, syntax: unaryArg.expression))
      }

      switch opToken.text {
      case ">=":
        return (
          active: actualVersion >= version,
          syntaxErrorsAllowed: fn.syntaxErrorsAllowed,
          diagnostics: extraDiagnostics
        )
      case "<":
        return (
          active: actualVersion < version,
          syntaxErrorsAllowed: fn.syntaxErrorsAllowed,
          diagnostics: extraDiagnostics
        )
      default:
        return recordError(.unsupportedVersionOperator(name: fnName, operator: opToken))
      }
    }

    switch fn {
    case .hasAttribute:
      return doSingleIdentifierArgumentCheck(configuration.hasAttribute, role: "attribute")

    case .hasFeature:
      return doSingleIdentifierArgumentCheck(configuration.hasFeature, role: "feature")

    case .os:
      return doSingleIdentifierArgumentCheck(configuration.isActiveTargetOS, role: "operating system")

    case .arch:
      return doSingleIdentifierArgumentCheck(configuration.isActiveTargetArchitecture, role: "architecture")

    case .targetEnvironment:
      return doSingleIdentifierArgumentCheck(configuration.isActiveTargetEnvironment, role: "environment")

    case ._runtime:
      return doSingleIdentifierArgumentCheck(configuration.isActiveTargetRuntime, role: "runtime")

    case ._ptrauth:
      return doSingleIdentifierArgumentCheck(
        configuration.isActiveTargetPointerAuthentication,
        role: "pointer authentication scheme"
      )

    case .defined:
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        let arg = argExpr.simpleIdentifierExpr?.name
      else {
        return recordError(.unknownExpression(condition))
      }
      extraDiagnostics.append(
        IfConfigDiagnostic.unexpectedDefined(syntax: condition, argument: arg).asDiagnostic
      )
      return checkConfiguration(at: condition) {
        (active: try configuration.isCustomConditionSet(name: arg), syntaxErrorsAllowed: false)
      }

    case ._endian:
      // Ensure that we have a single argument that is a simple identifier.
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        let arg = argExpr.simpleIdentifierExpr?.name
      else {
        return recordError(
          .requiresUnlabeledArgument(
            name: fnName,
            role: "endianness ('big' or 'little')",
            syntax: ExprSyntax(call)
          )
        )
      }

      // The argument needs to be either "little" or "big". Otherwise, we assume
      // it fails.
      let isActive: Bool
      if let expectedEndianness = Endianness(rawValue: arg) {
        isActive = configuration.endianness == expectedEndianness
      } else {
        // Complain about unknown endianness
        extraDiagnostics.append(
          IfConfigDiagnostic.endiannessDoesNotMatch(syntax: argExpr, argument: arg)
            .asDiagnostic
        )

        isActive = false
      }

      return (
        active: isActive,
        syntaxErrorsAllowed: fn.syntaxErrorsAllowed,
        diagnostics: extraDiagnostics
      )

    case ._pointerBitWidth, ._hasAtomicBitWidth:
      // Ensure that we have a single argument that is a simple identifier, which
      // is an underscore followed by an integer.
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        let arg = argExpr.simpleIdentifierExpr?.name,
        let argFirst = arg.first,
        argFirst == "_",
        let expectedBitWidth = Int(arg.dropFirst())
      else {
        return recordError(
          .requiresUnlabeledArgument(
            name: fnName,
            role: "bit width ('_' followed by an integer)",
            syntax: ExprSyntax(call)
          )
        )
      }

      let active: Bool
      if fn == ._pointerBitWidth {
        active = configuration.targetPointerBitWidth == expectedBitWidth
      } else if fn == ._hasAtomicBitWidth {
        active = configuration.targetAtomicBitWidths.contains(expectedBitWidth)
      } else {
        fatalError("extraneous case above not handled")
      }

      return (active: active, syntaxErrorsAllowed: fn.syntaxErrorsAllowed, diagnostics: extraDiagnostics)

    case .swift:
      return doVersionComparisonCheck(configuration.languageVersion)

    case .compiler:
      return doVersionComparisonCheck(configuration.compilerVersion)

    case ._compiler_version:
      // Argument is a single unlabeled argument containing a string
      // literal.
      guard let argExpr = call.arguments.singleUnlabeledExpression,
        let stringLiteral = argExpr.as(StringLiteralExprSyntax.self),
        stringLiteral.segments.count == 1,
        let segment = stringLiteral.segments.first,
        case .stringSegment(let stringSegment) = segment
      else {
        return doVersionComparisonCheck(configuration.compilerVersion)
      }

      let versionString = stringSegment.content.text
      let expectedVersion: VersionTuple
      do {
        expectedVersion = try VersionTuple.parseCompilerBuildVersion(
          versionString,
          argExpr,
          extraDiagnostics: &extraDiagnostics
        )
      } catch {
        return recordError(error, at: stringSegment.content)
      }

      return (
        active: configuration.compilerVersion >= expectedVersion,
        syntaxErrorsAllowed: fn.syntaxErrorsAllowed,
        diagnostics: extraDiagnostics
      )

    case .canImport:
      // Retrieve the first argument, which must not have a label. This is
      // the module import path.
      guard let firstArg = call.arguments.first,
        firstArg.label == nil
      else {
        return recordError(.canImportMissingModule(syntax: ExprSyntax(call)))
      }

      if call.arguments.count > 2 {
        return recordError(.canImportTwoParameters(syntax: ExprSyntax(call)))
      }

      // Extract the import path.
      let importPath: [(TokenSyntax, String)]
      do {
        importPath = try extractImportPath(firstArg.expression)
      } catch {
        return recordError(error, at: firstArg.expression)
      }

      // If there is a second argument, it shall have the label _version or
      // _underlyingVersion.
      let version: CanImportVersion
      if let secondArg = call.arguments.dropFirst().first {
        if secondArg.label?.text != "_version" && secondArg.label?.text != "_underlyingVersion" {
          return recordError(.canImportLabel(syntax: secondArg.expression))
        }

        let versionText: String
        if let stringLiteral = secondArg.expression.as(StringLiteralExprSyntax.self),
          stringLiteral.segments.count == 1,
          let firstSegment = stringLiteral.segments.first,
          case .stringSegment(let stringSegment) = firstSegment
        {
          versionText = stringSegment.content.text
        } else {
          versionText = secondArg.expression.trimmedDescription
        }

        guard var versionTuple = VersionTuple(parsing: versionText) else {
          return recordError(
            .invalidVersionOperand(name: "canImport", syntax: secondArg.expression)
          )
        }

        // Remove excess components from the version,
        if versionTuple.components.count > 4 {
          // Remove excess components.
          versionTuple.components.removeSubrange(4...)

          // Warn that we did this.
          extraDiagnostics.append(
            IfConfigDiagnostic.ignoredTrailingComponents(
              version: versionTuple,
              syntax: secondArg.expression
            ).asDiagnostic
          )
        }

        if secondArg.label?.text == "_version" {
          version = .version(versionTuple)
        } else {
          assert(secondArg.label?.text == "_underlyingVersion")
          version = .underlyingVersion(versionTuple)
        }
      } else {
        version = .unversioned
      }

      return checkConfiguration(at: call) {
        (
          active: try configuration.canImport(
            importPath: importPath,
            version: version
          ),
          syntaxErrorsAllowed: fn.syntaxErrorsAllowed
        )
      }
    }
  }

  return recordError(.unknownExpression(condition))
}

extension SyntaxProtocol {
  /// Determine whether this expression node is an "outermost" #if condition,
  /// meaning that it is not nested within some kind of expression like && or
  /// ||.
  fileprivate var isOutermostIfCondition: Bool {
    // If there is no parent, it's the outermost condition.
    guard let parent = self.parent else {
      return true
    }

    // If we hit the #if condition clause, it's the outermost condition.
    if parent.is(IfConfigClauseSyntax.self) {
      return true
    }

    // We found an infix operator, so this is not an outermost #if condition.
    if parent.is(InfixOperatorExprSyntax.self) {
      return false
    }

    // Keep looking up the syntax tree.
    return parent.isOutermostIfCondition
  }
}

/// Given an expression with the expected form A.B.C, extract the import path
/// ["A", "B", "C"] from it with the token syntax nodes for each name.
/// Throws an error if the expression doesn't match this form.
private func extractImportPath(_ expression: some ExprSyntaxProtocol) throws -> [(TokenSyntax, String)] {
  // Member access.
  if let memberAccess = expression.as(MemberAccessExprSyntax.self),
    let base = memberAccess.base,
    let memberName = memberAccess.declName.simpleIdentifier?.name
  {
    return try extractImportPath(base) + [(memberAccess.declName.baseName, memberName)]
  }

  // Declaration reference.
  if let declRef = expression.as(DeclReferenceExprSyntax.self),
    let name = declRef.simpleIdentifier?.name
  {
    return [(declRef.baseName, name)]
  }

  throw IfConfigDiagnostic.expectedModuleName(syntax: ExprSyntax(expression))
}

/// Determine whether the given condition only involves disjunctions that
/// check the given config function against one of the provided values.
///
/// For example, this will match a condition like `os(iOS) ||  os(tvOS)`
/// when passed `IfConfigFunctions.os` and `["iOS", "tvOS"]`.
private func isConditionDisjunction(
  _ condition: some ExprSyntaxProtocol,
  function: IfConfigFunctions,
  anyOf values: [String]
) -> Bool {
  // Recurse into disjunctions. Both sides need to match.
  if let binOp = condition.as(InfixOperatorExprSyntax.self),
    let op = binOp.operator.as(BinaryOperatorExprSyntax.self),
    op.operator.text == "||"
  {
    return isConditionDisjunction(binOp.leftOperand, function: function, anyOf: values)
      && isConditionDisjunction(binOp.rightOperand, function: function, anyOf: values)
  }

  // Look through parentheses.
  if let tuple = condition.as(TupleExprSyntax.self), tuple.isParentheses,
    let element = tuple.elements.first
  {
    return isConditionDisjunction(element.expression, function: function, anyOf: values)
  }

  // If we have a call to this function, check whether the argument is one of
  // the acceptable values.
  if let call = condition.as(FunctionCallExprSyntax.self),
    let fnName = call.calledExpression.simpleIdentifierExpr?.name,
    let callFn = IfConfigFunctions(rawValue: fnName),
    callFn == function,
    let argExpr = call.arguments.singleUnlabeledExpression,
    let arg = argExpr.simpleIdentifierExpr?.name
  {
    return values.contains(arg)
  }

  return false
}

/// If this binary operator looks like it could be replaced by a
/// targetEnvironment(simulator) check, produce a diagnostic that does so.
///
/// For example, this checks for conditions like:
///
/// ```
/// #if (os(iOS) ||  os(tvOS)) && (arch(i386) || arch(x86_64))
/// ```
///
/// which should be replaced with
///
/// ```
/// #if targetEnvironment(simulator)
/// ```
private func diagnoseLikelySimulatorEnvironmentTest(
  _ binOp: InfixOperatorExprSyntax
) -> Diagnostic? {
  guard let op = binOp.operator.as(BinaryOperatorExprSyntax.self),
    op.operator.text == "&&"
  else {
    return nil
  }

  func isSimulatorPlatformOSTest(_ condition: ExprSyntax) -> Bool {
    return isConditionDisjunction(condition, function: .os, anyOf: ["iOS", "tvOS", "watchOS"])
  }

  func isSimulatorPlatformArchTest(_ condition: ExprSyntax) -> Bool {
    return isConditionDisjunction(condition, function: .arch, anyOf: ["i386", "x86_64"])
  }

  guard
    (isSimulatorPlatformOSTest(binOp.leftOperand) && isSimulatorPlatformArchTest(binOp.rightOperand))
      || (isSimulatorPlatformOSTest(binOp.rightOperand) && isSimulatorPlatformArchTest(binOp.leftOperand))
  else {
    return nil
  }

  return IfConfigDiagnostic.likelySimulatorPlatform(syntax: ExprSyntax(binOp)).asDiagnostic
}

/// If this identifier looks like it is a `TARGET_OS_*` compilation condition,
/// produce a diagnostic that suggests replacing it with the `os(*)` syntax.
///
/// For example, this checks for conditions like:
///
/// ```
/// #if TARGET_OS_IOS
/// ```
///
/// which should be replaced with
///
/// ```
/// #if os(iOS)
/// ```
private func diagnoseLikelyTargetOSTest(
  at reference: DeclReferenceExprSyntax,
  name: String
) -> Diagnostic? {
  let prefix = "TARGET_OS_"
  guard name.hasPrefix(prefix) else { return nil }
  let osName = String(name.dropFirst(prefix.count))

  if unmappedTargetOSNames.contains(osName) {
    return IfConfigDiagnostic.likelyTargetOS(syntax: ExprSyntax(reference), replacement: nil).asDiagnostic
  }

  guard let replacement = targetOSNameMap[osName] else { return nil }

  return IfConfigDiagnostic.likelyTargetOS(syntax: ExprSyntax(reference), replacement: replacement).asDiagnostic
}

// TARGET_OS_* macros that don’t have a direct Swift equivalent
private let unmappedTargetOSNames = ["WIN32", "UNIX", "MAC", "IPHONE", "EMBEDDED"]
private let targetOSNameMap: [String: ExprSyntax] = [
  "WINDOWS": "os(Windows)",
  "LINUX": "os(Linux)",
  "OSX": "os(macOS)",
  "IOS": "os(iOS)",
  "MACCATALYST": "targetEnvironment(macCatalyst)",
  "TV": "os(tvOS)",
  "WATCH": "os(watchOS)",
  "VISION": "os(visionOS)",
  "SIMULATOR": "targetEnvironment(simulator)",
]

extension IfConfigClauseSyntax {
  /// Fold the operators within an #if condition, turning sequence expressions
  /// involving the various allowed operators (&&, ||, !) into well-structured
  /// binary operators.
  static func foldOperators(
    _ condition: some ExprSyntaxProtocol
  ) -> (folded: ExprSyntax, diagnostics: [Diagnostic]) {
    var foldingDiagnostics: [Diagnostic] = []
    let foldedCondition = OperatorTable.logicalOperators.foldAll(condition) { error in
      // Replace the "unknown infix operator" diagnostic with a custom one
      // that mentions that only '&&' and '||' are allowed.
      if case .missingOperator(_, referencedFrom: let syntax) = error,
        let binOp = syntax.parent?.as(BinaryOperatorExprSyntax.self)
      {

        foldingDiagnostics.append(
          IfConfigDiagnostic.badInfixOperator(syntax: ExprSyntax(binOp)).asDiagnostic
        )
        return
      }

      foldingDiagnostics.append(contentsOf: error.asDiagnostics(at: condition))
    }.cast(ExprSyntax.self)
    return (folded: foldedCondition, diagnostics: foldingDiagnostics)
  }

  /// Determine whether the given expression, when used as the condition in
  /// an inactive `#if` clause, implies that syntax errors are permitted within
  /// that region.
  @_spi(Compiler)
  public static func syntaxErrorsAllowed(
    _ condition: some ExprSyntaxProtocol
  ) -> (syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
    let (foldedCondition, foldingDiagnostics) = IfConfigClauseSyntax.foldOperators(condition)

    return (
      !foldingDiagnostics.isEmpty || foldedCondition.allowsSyntaxErrorsFolded,
      foldingDiagnostics
    )
  }
}

extension ExprSyntaxProtocol {
  /// Determine whether this expression, when used as a condition within a #if
  /// that evaluates false, implies that the code contained in that `#if`
  ///
  /// Check whether of allowsSyntaxErrors(_:) that assumes that inputs have
  /// already been operator-folded.
  var allowsSyntaxErrorsFolded: Bool {
    // Logical '!'.
    if let prefixOp = self.as(PrefixOperatorExprSyntax.self),
      prefixOp.operator.text == "!"
    {
      return prefixOp.expression.allowsSyntaxErrorsFolded
    }

    // Logical '&&' and '||'.
    if let binOp = self.as(InfixOperatorExprSyntax.self),
      let op = binOp.operator.as(BinaryOperatorExprSyntax.self)
    {
      switch op.operator.text {
      case "&&":
        return binOp.leftOperand.allowsSyntaxErrorsFolded || binOp.rightOperand.allowsSyntaxErrorsFolded
      case "||":
        return binOp.leftOperand.allowsSyntaxErrorsFolded && binOp.rightOperand.allowsSyntaxErrorsFolded
      default:
        return false
      }
    }

    // Look through parentheses.
    if let tuple = self.as(TupleExprSyntax.self), tuple.isParentheses,
      let element = tuple.elements.first
    {
      return element.expression.allowsSyntaxErrorsFolded
    }

    // Call syntax is for operations.
    if let call = self.as(FunctionCallExprSyntax.self),
      let fnName = call.calledExpression.simpleIdentifierExpr?.name,
      let fn = IfConfigFunctions(rawValue: fnName)
    {
      return fn.syntaxErrorsAllowed
    }

    return false
  }
}

/// Build configuration adaptor that suppresses calls to canImport, which
/// can have side effects. This is somewhat of a hack for the compiler.
private struct CanImportSuppressingBuildConfiguration<Other: BuildConfiguration>: BuildConfiguration {
  var other: Other

  func isCustomConditionSet(name: String) throws -> Bool {
    return try other.isCustomConditionSet(name: name)
  }

  func hasFeature(name: String) throws -> Bool {
    return try other.hasFeature(name: name)
  }

  func hasAttribute(name: String) throws -> Bool {
    return try other.hasAttribute(name: name)
  }

  func canImport(importPath: [(TokenSyntax, String)], version: CanImportVersion) throws -> Bool {
    return false
  }

  func isActiveTargetOS(name: String) throws -> Bool {
    return try other.isActiveTargetOS(name: name)
  }

  func isActiveTargetArchitecture(name: String) throws -> Bool {
    return try other.isActiveTargetArchitecture(name: name)
  }

  func isActiveTargetEnvironment(name: String) throws -> Bool {
    return try other.isActiveTargetEnvironment(name: name)
  }

  func isActiveTargetRuntime(name: String) throws -> Bool {
    return try other.isActiveTargetRuntime(name: name)
  }

  func isActiveTargetPointerAuthentication(name: String) throws -> Bool {
    return try other.isActiveTargetPointerAuthentication(name: name)
  }

  var targetPointerBitWidth: Int { return other.targetPointerBitWidth }

  var targetAtomicBitWidths: [Int] { return other.targetAtomicBitWidths }

  var endianness: Endianness { return other.endianness }

  var languageVersion: VersionTuple { return other.languageVersion }

  var compilerVersion: VersionTuple { return other.compilerVersion }
}

enum IfConfigFunctions: String {
  /// A check for a specific attribute via `hasAttribute(<name>)`.
  case hasAttribute

  /// A check for a specific named feature via `hasFeature(<name>)`.
  case hasFeature

  /// A check for the Swift language version via `swift(>=version).`
  case swift

  /// A check for the Swift compiler version via `compiler(>=version)`.
  case compiler

  /// A check to determine whether a given module can be imported via `canImport(<import path>)`.
  case canImport

  /// A check for the target Operating System kind (e.g., Linux, iOS) via `os(<name>)`
  case os

  /// A check for the target architecture (e.g., arm64, x86_64) via `arch(<name>)`.
  case arch

  /// A check for the target environment (e.g., simulator) via `targetEnvironment(<environment>)`.
  case targetEnvironment

  /// A check to determine whether the platform supports atomic operations
  /// with the given bitwidth, e.g., `_hasAtomicBitWidth(_64)`.
  case _hasAtomicBitWidth

  /// A historical check against a specific compiler build version via `_compiler_version("<version>")`.
  case _compiler_version

  /// A check for the target endianness (e.g., big or little) via `_endian(big|little)`.
  case _endian

  /// A check for the target bit width of a pointer (e.g., _64)
  case _pointerBitWidth

  /// A check for the target runtime paired with the Swift runtime (e.g., _ObjC)
  /// via `_runtime(<name>)`.
  case _runtime

  /// A check for the target's pointer authentication scheme (e.g., _arm64e)
  /// via `_ptrauth(<name>)`.
  case _ptrauth

  /// An unsupported function used by C preprocessor macros (e.g. `#if defined(FOO)`)
  case defined

  /// Whether uses of this function consitutes a check that guards new syntax.
  /// When such a check fails, the compiler should not diagnose syntax errors
  /// within the covered block.
  var syntaxErrorsAllowed: Bool {
    switch self {
    case .swift, .compiler, ._compiler_version:
      return true

    case .hasAttribute, .hasFeature, .canImport, .os, .arch, .targetEnvironment,
      ._hasAtomicBitWidth, ._endian, ._pointerBitWidth, ._runtime, ._ptrauth, .defined:
      return false
    }
  }
}

public enum IfConfigRegionState {
  /// The region is not part of the compiled program and is not even parsed,
  /// and therefore many contain syntax that is invalid.
  case unparsed
  /// The region is parsed but is not part of the compiled program.
  case inactive
  /// The region is active and is part of the compiled program.
  case active

  /// Evaluate the given `#if` condition using the given build configuration
  /// to determine its state, whether syntax errors in inactive conditions are
  /// permitted, and to identify any problems encountered along the way.
  public static func evaluating(
    _ condition: some ExprSyntaxProtocol,
    in configuration: some BuildConfiguration
  ) -> (state: IfConfigRegionState, syntaxErrorsAllowed: Bool, diagnostics: [Diagnostic]) {
    // Apply operator folding for !/&&/||.
    let (foldedCondition, foldingDiagnostics) = IfConfigClauseSyntax.foldOperators(condition)

    let (active, syntaxErrorsAllowed, evalDiagnostics) = evaluateIfConfig(
      condition: foldedCondition,
      configuration: configuration
    )

    let diagnostics = foldingDiagnostics + evalDiagnostics
    switch (active, syntaxErrorsAllowed) {
    case (true, _): return (.active, syntaxErrorsAllowed, diagnostics)
    case (false, false): return (.inactive, syntaxErrorsAllowed, diagnostics)
    case (false, true): return (.unparsed, syntaxErrorsAllowed, diagnostics)
    }
  }
}

extension BooleanLiteralExprSyntax {
  var literalValue: Bool {
    return literal.tokenKind == .keyword(.true)
  }
}

extension TupleExprSyntax {
  /// Whether this tuple is a parenthesized expression, e.g., (x).
  var isParentheses: Bool {
    return elements.singleUnlabeledExpression != nil
  }
}

extension LabeledExprListSyntax {
  /// If this list is a single, unlabeled expression, return it.
  var singleUnlabeledExpression: ExprSyntax? {
    guard count == 1, let element = first, element.label == nil else { return nil }
    return element.expression
  }
}

extension ExprSyntax {
  /// Whether this is a simple identifier expression and, if so, what the identifier string is.
  var simpleIdentifierExpr: Identifier? {
    guard let identExpr = self.as(DeclReferenceExprSyntax.self) else {
      return nil
    }

    return identExpr.simpleIdentifier
  }
}

extension DeclReferenceExprSyntax {
  /// If this declaration reference is a simple identifier, return that
  /// string.
  var simpleIdentifier: Identifier? {
    guard argumentNames == nil else {
      return nil
    }

    return baseName.identifier
  }
}

extension SyntaxProtocol {
  /// Determine whether the given syntax node is active within the given build configuration.
  ///
  /// This function evaluates the enclosing stack of `#if` conditions to determine whether the
  /// given node is active in the program when it is compiled with the given build configuration.
  ///
  /// For example, given code like the following:
  /// #if DEBUG
  ///   #if A
  ///    func f()
  ///   #elseif B
  ///     func g()
  ///   #elseif compiler(>= 12.0)
  ///     please print the number after 41
  ///   #endif
  /// #endif
  ///
  /// a call to `isActive` on the syntax node for the function `g` would return `active` when the
  /// configuration options `DEBUG` and `B` are provided, but `A` is not.
  ///
  /// If the compiler version is smaller than 12.0, then `isActive` on any of the tokens within
  /// that `#elseif` block would return "unparsed", because that syntax should not (conceptually)
  /// be parsed.
  ///
  /// Note that this function requires processing all #ifs from the root node
  /// of the syntax tree down to the current node. If performing more than a
  /// small number of `isActive(_:)` queries, please form a `ConfiguredRegions`
  /// instance and use `ConfiguredRegions.isActive(_:)` instead.
  public func isActive(
    in configuration: some BuildConfiguration
  ) -> (state: IfConfigRegionState, diagnostics: [Diagnostic]) {
    let configuredRegions = root.configuredRegions(in: configuration)
    return (configuredRegions.isActive(self), configuredRegions.diagnostics)
  }
}

public struct VersionTuple: Sendable {
  /// The components of the version tuple, start with the major version.
  public var components: [Int]

  /// Create a version tuple from a non-empty array of components.
  public init(components: [Int]) {
    precondition(!components.isEmpty)
    self.components = components
  }

  /// Create a version tuple from its components.
  public init(_ firstComponent: Int, _ remainingComponents: Int...) {
    self.components = []
    self.components.append(firstComponent)
    self.components.append(contentsOf: remainingComponents)
  }

  /// Parse a string into a version tuple, returning `nil` if any errors were
  /// present.
  public init?(parsing string: String) {
    self.components = []

    for componentText in string.split(separator: ".", omittingEmptySubsequences: false) {
      guard let component = Int(componentText) else {
        return nil
      }

      components.append(component)
    }

    if components.isEmpty { return nil }
  }

  /// Normalize the version tuple by removing trailing zeroes.
  var normalized: VersionTuple {
    var newComponents = components
    while newComponents.count > 1 && newComponents.last == 0 {
      newComponents.removeLast()
    }

    return VersionTuple(components: newComponents)
  }
}

extension VersionTuple: Equatable, Hashable {}

extension VersionTuple: Comparable {
  public static func < (lhs: VersionTuple, rhs: VersionTuple) -> Bool {
    return lhs.normalized.components.lexicographicallyPrecedes(rhs.normalized.components)
  }
}

extension VersionTuple: CustomStringConvertible {
  public var description: String {
    return components.map { String($0) }.joined(separator: ".")
  }
}

extension VersionTuple {
  /// Parse a compiler build version of the form "5007.*.1.2.3*", which is
  /// used by an older if configuration form `_compiler_version("...")`.
  /// - Parameters:
  ///   - versionString: The version string for the compiler build version that
  ///   we are parsing.
  ///   - versionSyntax: The syntax node that contains the version string, used
  ///   only for diagnostic purposes.
  static func parseCompilerBuildVersion(
    _ versionString: String,
    _ versionSyntax: ExprSyntax
  ) -> (version: VersionTuple?, diagnostics: [Diagnostic]) {
    var extraDiagnostics: [Diagnostic] = []
    let version: VersionTuple?
    do {
      version = try parseCompilerBuildVersion(versionString, versionSyntax, extraDiagnostics: &extraDiagnostics)
    } catch {
      version = nil
      extraDiagnostics.append(contentsOf: error.asDiagnostics(at: versionSyntax))
    }

    return (version, extraDiagnostics)
  }

  /// Parse a compiler build version of the form "5007.*.1.2.3*", which is
  /// used by an older if configuration form `_compiler_version("...")`.
  /// - Parameters:
  ///   - versionString: The version string for the compiler build version that
  ///   we are parsing.
  ///   - versionSyntax: The syntax node that contains the version string, used
  ///   only for diagnostic purposes.
  static func parseCompilerBuildVersion(
    _ versionString: String,
    _ versionSyntax: ExprSyntax,
    extraDiagnostics: inout [Diagnostic]
  ) throws -> VersionTuple {
    var components: [Int] = []

    // Version value are separated by periods.
    let componentStrings = versionString.split(separator: ".", omittingEmptySubsequences: false)

    /// Record a component after checking its value.
    func recordComponent(_ value: Int) throws {
      let limit = components.isEmpty ? 9223371 : 999
      if value < 0 || value > limit {
        throw IfConfigDiagnostic.compilerVersionOutOfRange(value: value, upperLimit: limit, syntax: versionSyntax)
      }

      components.append(value)
    }

    // Parse the components into version values.
    for (index, componentString) in componentStrings.enumerated() {
      // Check ahead of time for empty version components
      if componentString.isEmpty {
        throw IfConfigDiagnostic.emptyVersionComponent(syntax: versionSyntax)
      }

      // The second component is always "*", and is never used for comparison.
      if index == 1 {
        if componentString != "*" {
          extraDiagnostics.append(
            IfConfigDiagnostic.compilerVersionSecondComponentNotWildcard(syntax: versionSyntax).asDiagnostic
          )
        }
        try recordComponent(0)
        continue
      }

      // Every other component must be an integer value.
      guard let component = Int(componentString) else {
        throw IfConfigDiagnostic.invalidVersionOperand(name: "_compiler_version", syntax: versionSyntax)
      }

      try recordComponent(component)
    }

    // Only allowed to specify up to 5 version components.
    if components.count > 5 {
      throw IfConfigDiagnostic.compilerVersionTooManyComponents(syntax: versionSyntax)
    }

    // In the beginning, '_compiler_version(string-literal)' was designed for a
    // different version scheme where the major was fairly large and the minor
    // was ignored; now we use one where the minor is significant and major and
    // minor match the Swift language version. Specifically, majors 600-1300
    // were used for Swift 1.0-5.5 (based on clang versions), but then we reset
    // the numbering based on Swift versions, so 5.6 had major 5. We assume
    // that majors below 600 use the new scheme and equal/above it use the old
    // scheme.
    //
    // However, we want the string literal variant of '_compiler_version' to
    // maintain source compatibility with old checks; that means checks for new
    // versions have to be written so that old compilers will think they represent
    // newer versions, while new compilers have to interpret old version number
    // strings in a way that will compare correctly to the new versions compiled
    // into them.
    //
    // To achieve this, modern compilers divide the major by 1000 and overwrite
    // the wildcard component with the remainder, effectively shifting the last
    // three digits of the major into the minor, before comparing it to the
    // compiler version:
    //
    //     _compiler_version("5007.*.1.2.3") -> 5.7.1.2.3
    //     _compiler_version("1300.*.1.2.3") -> 1.300.1.2.3 (smaller than 5.6)
    //     _compiler_version( "600.*.1.2.3") -> 0.600.1.2.3 (smaller than 5.6)
    //
    // So if you want to specify a 5.7.z.a.b version, we ask users to either
    // write it as 5007.*.z.a.b, or to use the new 'compiler(>= version)'
    // syntax instead, which does not perform this conversion.
    if !components.isEmpty {
      if components.count > 1 {
        components[1] = components[0] % 1000
      }
      components[0] = components[0] / 1000
    }

    return VersionTuple(components: components)
  }
}
