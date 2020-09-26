import io.github.mahdyne.HackerRank._
import org.scalatest.{FunSuite, Matchers}

/**
 * @author mahdyne on 9/9/19.
 */
class HackerRankTest extends FunSuite with Matchers{
  test("gradingStudents"){
    val a=Array(4,73,67,38,33)
    assert(gradingStudents(a)===Array(4,75,67,40,33))
  }
  test("countApplesAndOranges"){
    val s=7
    val t=11
    val a=5
    val b=15
    val applesD=Array(-2,2,1)
    val orangesD=Array(5,-6)
    assert(countApplesAndOranges(s,t,a,b,applesD,orangesD)===Array(1,1))
  }
  test("getTotalX"){
    val a=Array(2,4)
    val b=Array(16,32,96)
    assert(getTotalX(a,b)===3)
  }
  test("breakingRecords"){
    val b=Array(10,5,20,20,4,5,2,25,1)
    assert(breakingRecords(b)===Array(2,4))
  }
  test("birthday"){
    val b=Array(1,2,1,3,2)
    assert(birthday(b,3,2)===2)
  }
  test("kangaroo"){
    assert(kangaroo(0,2,5,3)==="NO")
  }
  test("divisibleSum"){
    assert(divisibleSumPairs(6,3,Array(1,3,2,6,1,2))===5)
  }
  test("migratoryBird"){
    assert(migratoryBirds(Array(1,3,2,6,1,2))===1)
  }
  test("dayOfProgrammer"){
    assert(dayOfProgrammer(2000)==="12.09.2000")
  }
  test("bonAppetit"){
    assert(bonAppetit(Array(3,10,2,9),1,12)==="5")
  }
  test("repeatedString"){
    val s = "epsxyyflvrrrxzvnoenvpegvuonodjoxfwdmcvwctmekpsnamchznsoxaklzjgrqruyzavshfbmuhdwwmpbkwcuomqhiyvuztwvq"
    val n:Long=549382313570L
    assert(repeatedString(s,n)===16481469408L)
  }
  test("hourglassSum"){
    val arr=Array(Array(1, 1, 1, 0, 0, 0), Array(0, 1, 0, 0, 0, 0), Array(1, 1, 1, 0, 0, 0), Array(0, 0, 2, 4, 4, 0), Array(0, 0, 0, 2, 0, 0), Array(0, 0, 1, 2, 4, 0))
    assert(hourglassSum(arr)===19)
  }
  test("rotLeft"){
    val arr=Array(1,2,3,4,5)
    assert(rotLeft(arr,4)===Array(5,1,2,3,4))
  }
  test("rotLeftV2"){
    val arr=Array(1,2,3,4,5)
    assert(rotLeftV2(arr,4)===Array(5,1,2,3,4))
  }
  test("minimumSwaps2"){
    val arr=Array(7, 1, 3, 2, 4, 5, 6)
    assert(minimumSwaps2(arr)===5)
  }
  test("minimumSwaps3"){
    val arr=Array(7, 1, 3, 2, 4, 5, 6)
    assert(minimumSwaps3(arr)===5)
  }
  test("twoStrings"){
    val s1="hackerrankcommunity"
    val s2="cdecdecdecde"
    assert(twoStrings(s1,s2)==="YES")
  }
  test("checkMagazine"){
    val s1="hi all, here is surkhrud , im an engineer and i can see the beach from my home".split(" ")
    val s2="im an engineer and im from surkhrud".split(" ")
    assert(checkMagazine(s1,s2)==="NO")
  }
  test("sherlockAndAnagrams"){
    val s="ifailuhkqq"
    assert(sherlockAndAnagrams(s)===3)
  }
  test("gameOfThrones"){
    val s="cdcdcdcdeeeef"
    assert(gameOfThrones(s)==="YES")
  }
}