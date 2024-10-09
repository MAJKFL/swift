// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation

protocol P {
  associatedtype A // expected-note{{protocol requires nested type 'A'}}
  associatedtype B // expected-note{{protocol requires nested type 'B'}}
  func f1(_: A)
}

extension P {
  func f1(_: A) {}
}

extension P {
  func f1(_: B) {}
}

struct S: P { // expected-error{{type 'S' does not conform to protocol 'P'}} expected-note{{add stubs for conformance}}
  func f1(_: Int) {}
}

struct X0i<T : PSimple> { } // expected-error{{cannot find type 'PSimple' in scope}}

extension X0i {
  func g0(_: T) { }
}

extension X0i : P0 { } // expected-error{{cannot find type 'P0' in scope}}

extension X0i {
  func f0(_: T) { }
}
