package fpis.ch3

import org.scalatest.FunSuite

class TreeTestSuites extends FunSuite{
  test("tree size"){
    val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Branch(Leaf(4), Leaf(5)))
    assert(TreeUtils.size(t) == 9)
    assert(TreeUtils.sizeViaFold(t) == 9)
  }

  test("tree max value"){
    val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Branch(Leaf(4), Leaf(5)))
    assert(TreeUtils.maximum(t) == 5)
    assert(TreeUtils.maximumViaFold(t) == 5)
  }

  test("tree depth"){
    val t = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Branch(Leaf(4), Leaf(5)))
    assert(TreeUtils.depth(t) == 3)
    assert(TreeUtils.depthViaFold(t) == 3)
  }

  test("tree map"){
    val t = Branch(Branch(Branch(Leaf("a"), Leaf("bb")), Leaf("ccc")), Branch(Leaf("dddd"), Leaf("eeeee")))
    val t1 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Branch(Leaf(4), Leaf(5)))
    assert(TreeUtils.map(t)(_.length) == t1)
    assert(TreeUtils.mapViaFold(t)(_.length) == t1)
  }
}
