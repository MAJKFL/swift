// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation
class X {
  
  var arr = ["AB", "CD"]
  
  subscript(a: Int) -> String {
    get {
      return arr[a]
    }
    set(newValue) {
      arr[a] = newValue
    }
  }
}

class Y {
  var c = 123

  subscript(a: Int?, b: Int?, c: Int?) -> Int {
    let a = 1 // expected-note {{'a' previously declared here}}
    let b = 2

    guard let b = a else { return self.c } // expected-error {{initializer for conditional binding must have Optional type, not 'Int'}} expected-note {{'b' previously declared here}}

    let a = 3 // expected-error {{invalid redeclaration of 'a'}}
    let b = 4 // expected-error {{invalid redeclaration of 'b'}}

    guard let a = c else { return a }

    print(a, b)
    
    return a + b
  }
}
