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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class PascalSpec extends AnyFlatSpec with Matchers:

    "getCoefficient" should "give the right result for a few known inputs" in {
        // Some known values early in Pascal's triange stored in a Seq.
        // The format is ((n,k),answer)
        val known = Seq(((0,0),1), ((4,0),1), ((5,5),1), ((4,1),4), ((5,2),10))
        // Go over the known answers and check them
        // this is not exhaustive of course, but if you answers passes this
        // you can probably check them with the grader
        for ((n,k),v) <- known do
            withClue(s"For input n=$n, k=$k") {
                (getCoefficient(n,k)) shouldBe v
            }
    }

end PascalSpec
