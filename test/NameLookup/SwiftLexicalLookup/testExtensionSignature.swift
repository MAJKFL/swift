// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation

import SwiftShims

extension Unicode.SMTH: Sequence { // expected-error{{'SMTH' is not a member type of enum 'Swift.Unicode'}}

  internal consuming func makeIterator() -> Iterator {
    Iterator(source: source.makeIterator())
  }

  internal struct Iterator: IteratorProtocol {

    internal var source: Source.Iterator
    internal var normalizer: Unicode._NFCNormalizer

    internal init(source: Source.Iterator) {
      self.source = source
      if let strIter = source as? String.UnicodeScalarView.Iterator {
        self.normalizer = Unicode._NFCNormalizer(sourceString: strIter._guts)
      } else if let substrIter = source as? Substring.UnicodeScalarView.Iterator {
        self.normalizer = Unicode._NFCNormalizer(sourceString: substrIter._elements._wholeGuts)
      } else {
        self.normalizer = Unicode._NFCNormalizer()
      }
    }

    internal mutating func next() -> Unicode.Scalar? {
      normalizer.resume { source.next() } ?? normalizer.flush()
    }
  }
}
