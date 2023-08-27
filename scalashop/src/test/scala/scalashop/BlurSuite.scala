package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:
  test("test boxBlurKernel with predefined dataset") {
    val img: Img = Img(3, 3, Array(1, 2, 3, 4, 0, 6, 7, 8, 9))
    assertEquals(boxBlurKernel(img, 1, 1, 10), 5)
  }
