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





package pascal
  /**
   * Returns the k:th value in the n:th row of the Pascal's triangle
   * (that is, the binomial coefficient (n k), or the number of how many ways
   *  we can select k items from a set of n items).
   * For more information, see e.g.
   * - http://en.wikipedia.org/wiki/Pascal%27s_triangle
   * - http://en.wikipedia.org/wiki/Binomial_coefficient
   *
   * As this is a basic exercise on recursion,
   * you are not allowed to use var-declarations, Arrays or
   * any scala.collection.mutable data types.
   */
  def getCoefficient(n: Int, k: Int): Int =
    require(n >= 0)
    require(0 <= k && k <= n)
    
    if k == 0 || k == n then
      1
    else
      getCoefficient(n-1, k-1) + getCoefficient(n-1, k)
      //Derived from the formula:
      // (n k) = (n-1 k-1) + (n-1 k)
  end getCoefficient
  

