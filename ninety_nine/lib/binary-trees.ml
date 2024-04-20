(*
  A binary tree is either empty or it is composed of a root element and two
  successors, which are binary trees themselves.

  In OCaml, one can define a new type binary_tree that carries an arbitrary value
  of type 'a (thus is polymorphic) at each node.

  # type 'a binary_tree =
      | Empty
      | Node of 'a * 'a binary_tree * 'a binary_tree;;
  type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

  An example of tree carrying char data is:

  # let example_tree =
      Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
          Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;
  val example_tree : char binary_tree =
    Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
    Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))

  # let example_int_tree =
      Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
          Node (3, Empty, Node (6, Node (7, Empty, Empty), Empty)));;
  val example_int_tree : int binary_tree =
    Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
    Node (3, Empty, Node (6, Node (7, Empty, Empty), Empty)))

  In OCaml, the strict type discipline guarantees that, if you get a value of
  type binary_tree, then it must have been created with the two constructors
  Empty and Node.

---------------------------------------------------------------------------------

  55. Construct completely balanced binary trees. (medium)

  In a completely balanced binary tree, the following property holds for every
  node: The number of nodes in its left subtree and the number of nodes in its
  right subtree are almost equal, which means their difference is not greater
  than one.

  Write a function cbal_tree to construct completely balanced binary trees for a
  given number of nodes. The function should generate all solutions via
  backtracking. Put the letter 'x' as information into all nodes of the tree.

  # cbal_tree 4;;
  - : char binary_tree list =
  [ Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
  ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
  ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
  ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
  ]

  # List.length (cbal_tree 40);;
  - : int = 524288
*)

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let cbal_tree (n : int) : 'a binary_tree =
  if n == 0 then
    Empty

(*
56. Symmetric binary trees. (medium)

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a function is_symmetric to check whether a given binary tree is symmetric.

Hint: Write a function is_mirror first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

57. Binary search trees (dictionaries). (medium)

Construct a binary search tree from a list of integer numbers.

# construct [3; 2; 5; 7; 1];;
- : int binary_tree =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty)))

Then use this function to test the solution of the previous problem.

# is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);;
- : bool = true
# not (is_symmetric (construct [3; 2; 5; 7; 4]));;
- : bool = true

58. Generate-and-test paradigm. (medium)

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

# sym_cbal_trees 5;;
- : char binary_tree list =
[Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty))]

How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? What if the number is even? Write an appropriate function.

# List.length (sym_cbal_trees 57);;
- : int = 256
# List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20);;
- : (int * int) list =
[(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8);
 (18, 0); (19, 16); (20, 0)]

59. Construct height-balanced binary trees. (medium)

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Write a function hbal_tree to construct height-balanced binary trees for a given height. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

# let t = hbal_tree 3;;
val t : char binary_tree list =
  [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
# let x = 'x';;
val x : char = 'x'
# List.mem (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)),
                 Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)))) t;;
- : bool = true
# List.mem (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)),
                 Node (x, Node (x, Empty, Empty), Empty))) t;;
- : bool = true
# List.length t;;
- : int = 15

60. Construct height-balanced binary trees with a given number of nodes. (medium)

Consider a height-balanced binary tree of height h. What is the maximum number of nodes it can contain? Clearly, max_nodes = 2h - 1.

# let max_nodes h = 1 lsl h - 1;;
val max_nodes : int -> int = <fun>

However, what is the minimum number min_nodes? This question is more difficult. Try to find a recursive statement and turn it into a function min_nodes defined as follows: min_nodes h returns the minimum number of nodes in a height-balanced binary tree of height h.

On the other hand, we might ask: what are the minimum (resp. maximum) height H a height-balanced binary tree with N nodes can have? min_height (resp. max_height n) returns the minimum (resp. maximum) height of a height-balanced binary tree with n nodes.

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. hbal_tree_nodes n returns a list of all height-balanced binary tree with n nodes.

Find out how many height-balanced trees exist for n = 15.

# List.length (hbal_tree_nodes 15);;
- : int = 1553
# List.map hbal_tree_nodes [0; 1; 2; 3];;
- : char binary_tree list list =
[[Empty]; [Node ('x', Empty, Empty)];
 [Node ('x', Node ('x', Empty, Empty), Empty);
  Node ('x', Empty, Node ('x', Empty, Empty))];
 [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))]]

61. Count the leaves of a binary tree. (easy)

A leaf is a node with no successors. Write a function count_leaves to count them.

# count_leaves Empty;;
- : int = 0
# count_leaves example_tree;;
- : int = 3

61A. Collect the leaves of a binary tree in a list. (easy)

A leaf is a node with no successors. Write a function leaves to collect them in a list.

# leaves Empty;;
- : 'a list = []
# leaves example_tree;;
- : char list = ['d'; 'e'; 'g']

62. Collect the internal nodes of a binary tree in a list. (easy)

An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list.

# internals (Node ('a', Empty, Empty));;
- : char list = []
# internals example_tree;;
- : char list = ['b'; 'a'; 'c'; 'f']

62B. Collect the nodes at a given level in a list. (easy)

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list.

# at_level example_tree 2;;
- : char list = ['b'; 'c']
# at_level example_tree 5;;
- : char list = []

Using at_level it is easy to construct a function levelorder which creates the level-order sequence of the nodes. However, there are more efficient ways to do that.
63. Construct a complete binary tree. (medium)

A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i, note that we start counting the levels from 1 at the root). In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a function is_complete_binary_tree with the following specification: is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes.

# complete_binary_tree [1; 2; 3; 4; 5; 6];;
- : int binary_tree =
Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node (6, Empty, Empty), Empty))

64. Layout a binary tree (1). (medium)

As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration.

[Binary Tree Grid]

In this layout strategy, the position of a node v is obtained by the following two rules:

    x(v) is equal to the position of the node v in the inorder sequence;
    y(v) is equal to the depth of the node v in the tree.

In order to store the position of the nodes, we will enrich the value at each node with the position (x,y).

The tree pictured above is

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node ('n', Node ('k', Node ('c', leaf 'a',
                             Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                   leaf 'm'),
         Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty)),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)),
    Empty))

# layout_binary_tree_1 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 8, 1),
 Node (('k', 6, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('h', 5, 4),
    Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
  Node (('m', 7, 3), Empty, Empty)),
 Node (('u', 12, 2),
  Node (('p', 9, 3), Empty,
   Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
  Empty))

65. Layout a binary tree (2). (medium)

[Binary Tree Grid]

An alternative layout method is depicted in this illustration. Find out the rules and write the corresponding OCaml function.

Hint: On a given level, the horizontal distance between neighbouring nodes is constant.

The tree shown is

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node ('n', Node ('k', Node ('c', leaf 'a',
                             Node ('e', leaf 'd', leaf 'g')),
                   leaf 'm'),
         Node ('u', Node ('p', Empty, leaf 'q'), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('e', Node ('d', Empty, Empty), Node ('g', Empty, Empty))),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('q', Empty, Empty)), Empty))

# layout_binary_tree_2 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 15, 1),
 Node (('k', 7, 2),
  Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
    Node (('g', 6, 5), Empty, Empty))),
  Node (('m', 11, 3), Empty, Empty)),
 Node (('u', 23, 2),
  Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty))
# let example2_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node ('n', Empty,
         Node ('u', Node ('p', Empty, leaf 'q'), Empty));;
val example2_layout_tree : char binary_tree =
  Node ('n', Empty,
   Node ('u', Node ('p', Empty, Node ('q', Empty, Empty)), Empty))
# layout_binary_tree_2 example2_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 1, 1), Empty,
 Node (('u', 5, 2),
  Node (('p', 3, 3), Empty, Node (('q', 4, 4), Empty, Empty)), Empty))

66. Layout a binary tree (3). (hard)

[Binary Tree Grid]

Yet another layout strategy is shown in the above illustration. The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding predicate.

Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree? This is a difficult problem. Don't give up too early!

# layout_binary_tree_3 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 5, 1),
 Node (('k', 3, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 3, 4), Node (('d', 2, 5), Empty, Empty),
    Node (('g', 4, 5), Empty, Empty))),
  Node (('m', 4, 3), Empty, Empty)),
 Node (('u', 7, 2),
  Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty)), Empty))
# let example3_layout_tree =
    Node ('a', Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty))),
         Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)));;
val example3_layout_tree : char binary_tree =
  Node ('a', Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty))),
   Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)))
# layout_binary_tree_3 example3_layout_tree;;
- : (char * int * int) binary_tree =
Node (('a', 3, 1),
 Node (('b', 1, 2), Empty,
  Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty))),
 Node (('c', 5, 2), Empty,
  Node (('d', 6, 3), Node (('g', 5, 4), Empty, Empty), Empty)))

Which layout do you like most?
67. A string representation of binary trees. (medium)

[Binary Tree]

Somebody represents binary trees as strings of the following type (see example): "a(b(d,e),c(,f(g,)))".

    Write an OCaml function string_of_tree which generates this string representation, if the tree is given as usual (as Empty or Node(x,l,r) term). Then write a function tree_of_string which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single function tree_string which can be used in both directions.
    Write the same predicate tree_string using difference lists and a single predicate tree_dlist which does the conversion between a tree and a difference list in both directions.

For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string.

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
      (Node ('a', Node ('b', leaf 'd', leaf 'e'),
       Node ('c', Empty, Node ('f', leaf 'g', Empty))));;
val example_layout_tree : char binary_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
   Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
# string_of_tree example_layout_tree;;
- : string = "a(b(d,e),c(,f(g,)))"
# tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree;;
- : bool = true
# tree_of_string "";;
- : char binary_tree = Empty

68. Preorder and inorder sequences of binary trees. (medium)

We consider binary trees with nodes that are identified by single lower-case letters, as in the example of the previous problem.

    Write functions preorder and inorder that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in the previous problem.
    Can you use preorder from problem part 1 in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.
    If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a function pre_in_tree that does the job.
    Solve problems 1 to 3 using difference lists. Cool! Use the function timeit (defined in problem “Compare the two methods of calculating Euler's totient function.”) to compare the solutions.

What happens if the same character appears in more than one node. Try for instance pre_in_tree "aba" "baa".

# preorder (Node (1, Node (2, Empty, Empty), Empty));;
- : int list = [1; 2]
# preorder (Node (1, Empty, Node (2, Empty, Empty)));;
- : int list = [1; 2]
# let p = preorder example_tree;;
val p : char list = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g']
# let i = inorder example_tree;;
val i : char list = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f']
# pre_in_tree p i = example_tree;;
- : bool = true

Solution using difference lists.

  (* solution pending *)

69. Dotstring representation of binary trees. (medium)

We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem “A string representation of binary trees”. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem “A string representation of binary trees” is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a function tree_dotstring which does the conversion in both directions. Use difference lists.

  (* solution pending *)
*)
