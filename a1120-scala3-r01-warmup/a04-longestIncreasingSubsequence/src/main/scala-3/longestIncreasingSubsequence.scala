/*
* 
* This file is part of the CS-A1120 Programming 2 course materials at
* Aalto University in Spring 2025, and is for your personal use on that
* course only.
* Distribution of any parts of this file in any form, including posting or
* sharing on public or shared forums or repositories is *prohibited* and
* constitutes a violation of the code of conduct of the course.
* The programming exercises of CS-A1120 are individual and confidential
* assignments---this means that as a student taking the course you are
* allowed to individually and confidentially work with the material,
* to discuss and review the material with course staff, and submit the
* material for grading on course infrastructure.
* All other use - including, having other persons or programs
* (e.g. AI/LLM tools) working on or solving the exercises for you is
* forbidden, and constitutes a violation of the code of conduct of this
* course.
* 
*/





/*
 * Assignment: Longest increasing subsequence
 *
 * Sequences are a natural source of computational problems. One such
 * family of problems involves finding subsequences with specified
 * properties in a given sequence. This exercise asks you to write 
 * a program that, given a sequence
 *
 *   s(0), s(1), ..., s(n-1)
 *
 * of integers as input, finds a ___longest increasing subsequence___ 
 * of the sequence s.
 * 
 * For example, suppose we are given as input the sequence
 * 
 *    72, 16, 51, 17, 6, 21, 92, 59, 54, 78, 41, 33, 94, 
 *        85, 83, 56, 2, 46, 57, 44, 73, 6, 47, 47, 0.
 *
 * In this sequence, a longest increasing subsequence has length 7. 
 * One example of such an increasing subsequence is 
 *
 *    16 < 17 < 21 < 54 < 56 < 57 < 73.
 *
 * More generally, your program must be such that 
 * given a sequence
 *
 *   s(0), s(1), ..., s(n-1)
 *
 * of integers as input, the program returns a subsequence 
 *
 *   s(i_1), s(i_2), ..., s(i_k)
 *
 * that meets all of the following three properties:
 * 
 *   1. The positions that define the subsequence are increasing:
 *
 *        0 <= i_1 < i_2 < ... < i_k <= n-1
 *
 *   2. The subsequence is increasing:
 *
 *        s(i_1) < s(i_2) < ... < s(i_k)
 *
 *   3. The length k of the subsequence is as large as possible.
 *
 *
 * Hints:
 *
 * You can solve this exercise using __dynamic programming__ as a 
 * solution strategy. That is, you __tabulate__ solutions of carefully
 * designed subproblems and use these solutions to gradually build your
 * way up to solve larger and larger subproblems until you solve 
 * the original problem. Here are some more detailed hints:
 *
 * a) Suppose that for all i = 0, 1, ..., j-1 we know L(i), the length 
 *    of a longest increasing subsequence in s(0), s(1), ..., s(i)
 *    that terminates at position i.
 * b) Suppose we have already computed in an array the values
 *    L(0), L(1), ..., L(j-1). How can we make use of these values 
 *    to compute the next value L(j) ? 
 * c) Furthermore, suppose for each i we know p(i), the position
 *    prior to the last position i in a longest increasing subsequence 
 *    that terminates at position i.
 *    (Note that such a position p(i) need not always exist, however.)
 * d) How can we make use of the values p(i) for i <= j to determine
 *    a concrete increasing subsequence of length L(j) 
 *    that terminates at j ?
 * e) Design your program to complete the arrays L and p, one position 
 *    at a time.
 * f) Use the arrays L and p to return a concrete longest increasing
 *    subsequence in the given sequence s.
 *
 */

package longestIncreasingSubsequence

  import scala.compiletime.ops.double

  /** Returns a longest increasing subsequence in the sequence s.
   *  If there are multiple such subsequences, any subsequence will do. */

  def longestIncreasingSubsequence(s: Seq[Int]): Seq[Int] =
    require(s.length > 0)
    val n = s.length

    var bestLen = 1
    var bestEndIndex = 0

    // array of different lengths for each elem of s
      //each element storest the max len until that of s(i)
      //set to 1 since each elem is atleast a seq of 1
    val Lengths = Array.fill(n)(1)
    // array of previous index for each elem of s
      //each element stores the previous index for the best seq since it can jump over some elements
      //has the len of s since the best can be the same as s but not longer
      //set to -1 for detecting the termination for non n length seqs
    val Previous = Array.fill(n)(-1)

    //loop through each elem of s
    for i <- 0 until n do // for each element of s
      //loop through each elem of s before i to detect possble new sequence starts
      for j <- 0 until i do // for each element between 0 and i

        //check if the current elem is greater than the previous
        // a.k.a s(j) can be part of a seq with s(i)
        if s(i) > s(j) then
          //proceed to check if this is a new length record for current s(i)
          if Lengths(i) < Lengths(j) + 1 then //check that s(j) doesnt already have a longer seq
            Lengths(i) = Lengths(j) + 1 //update the best accumulated length 
            
            Previous(i) = j //update the previous index path for current best s(i) 

      //uodate the best seq length and position found so far
      if Lengths(i) > bestLen then
        bestLen = Lengths(i)
        bestEndIndex = i
      //proceed to loop through the next elem of s

    //build the best seq from the best position
    //this is done by recursively checking the previous index until -1
    def getSeq(n: Int, i: Int): Seq[Int] =
      if i == -1 then Seq() // if no previous index then terminate
      else getSeq(n, Previous(i)) :+ s(i) // else recursively get the previous index and append current s(i) to the seq

    getSeq(bestLen, bestEndIndex)
  end longestIncreasingSubsequence