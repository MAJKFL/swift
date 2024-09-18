// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation
class X {
  typealias A = Int
  
  class Foo<A, B: A> { // expected-note {{'A' previously declared here}} expected-note {{'B' previously declared here}} expected-error {{type 'B' constrained to non-protocol, non-class type 'A'}}
    let a: A = 123 // expected-error {{cannot convert value of type 'Int' to specified type 'A'}}
    
    func bar<A, B: A>() { // expected-warning {{generic parameter 'A' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}} expected-error {{generic parameter 'A' is not used in function signature}} expected-warning {{generic parameter 'B' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}} expected-error {{generic parameter 'B' is not used in function signature}} expected-error {{type 'B' constrained to non-protocol, non-class type 'A'}}
      let (a, b, c) = a + b // expected-note {{'b' declared here}} expected-error {{binary operator '+' cannot be applied to operands of type 'A' and 'String'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (ContinuousClock.Instant, Duration), (Double, Double), (Duration, Duration), (Float, Float), (Float16, Float16), (Int, Int), (Int16, Int16), (Int32, Int32), (Int64, Int64), (Int8, Int8), (String, String), (SuspendingClock.Instant, Duration), (UInt, UInt), (UInt16, UInt16), (UInt32, UInt32), (UInt64, UInt64), (UInt8, UInt8)}} expected-error {{use of local variable 'b' before its declaration}}
      
      guard let a, b else { return }
      
      let a = a + c

      guard let a, c else {
        print(a, b, c)
        return
      }
      
      print(a, b, c)

      class A {} // expected-error {{type 'A' cannot be nested in generic function 'bar()'}}
      
      class A {} // expected-error {{type 'A' cannot be nested in generic function 'bar()'}}
    }
  }
}
