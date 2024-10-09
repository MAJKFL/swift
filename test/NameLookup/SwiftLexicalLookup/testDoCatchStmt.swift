// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation

func testDo() {
  do {
    try canThrow() // expected-error{{cannot find 'canThrow' in scope}}
  } catch is UnknownError { // expected-error{{cannot find type 'UnknownError' in scope}}
    return
  } catch where error is UnknownError { // expected-error {{cannot find type 'UnknownError' in scope}}
    return
  } catch let error, {
    _ = error
    return
  } catch {
    return
  }

  LOOP: do {
    print("foo")
    if true {
      continue  LOOP
    }
  }
}
