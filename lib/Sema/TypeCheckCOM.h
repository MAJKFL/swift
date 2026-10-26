//===--- TypeCheckCOM.h - Type checking for COM interop -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKCOM_H
#define SWIFT_SEMA_TYPECHECKCOM_H

#include <cstdint>

namespace swift {
class ClassDecl;
class GenericContext;
class GenericSignature;
class ProtocolDecl;
class NominalTypeDecl;
class ProtocolConformance;
class Type;
enum class KnownProtocolKind : uint8_t;

namespace com {
/// Produce an implicit conformance of \p NTD to the COM protocol \p KP if it
/// is valid to do so.
ProtocolConformance *
deriveImplicitConformance(NominalTypeDecl *NTD, KnownProtocolKind KP);

/// Diagnose declaration-level restrictions on a native COM implementation.
void validateImplementation(ClassDecl *CD);

/// Validate COM-specific rules for a protocol declaration.
void validateProtocol(ProtocolDecl *PD);

/// Diagnose restrictions on an explicitly declared COM conformance.
void validateConformance(ProtocolConformance *conformance);

/// Whether \p type is a generic type constrained to a COM interface and to no
/// superclass. Such a value may hold a foreign interface pointer. \p sig
/// resolves a type parameter.
bool hasOpaqueRepresentation(Type type, GenericSignature sig);

/// Diagnose generic parameters of \p ctx that no type can satisfy because of
/// a COM interface constraint.
void validateGenericParams(GenericContext *ctx);
}
} // end namespace swift

#endif // SWIFT_SEMA_TYPECHECKCOM_H
