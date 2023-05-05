package com.oleksiihytsiv
package scala3snippets

final class ExampleSuite extends TestSuite:
  test("hello world") {
    forAll { (int: Int, string: String) =>
      expect(int === int, string === string)
    }
  }
