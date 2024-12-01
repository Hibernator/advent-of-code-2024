package ch.hibernator.adventofcode

import io.circe.*
import io.circe.generic.semiauto.deriveDecoder

import scala.io.Source
import scala.util.Using

trait SolutionBase[CommonResult]:
  def day: Int
  def calculateCommonResult(input: Seq[String]): CommonResult
  def solvePart1(input: Seq[String], commonResult: CommonResult): Long
  def solvePart2(input: Seq[String], commonResult: CommonResult): Long

  private val inputFileAddress = s"./src/main/resources/input/$day.txt"
//  private val inputFilePath = Paths.get(inputFileAddress)
//  private val inputFile = new File(inputFileAddress)
//  if !inputFile.exists() then inputFile.createNewFile()
//  if Files.size(inputFilePath) == 0 then {
//    val backend = HttpClientSyncBackend()
//
//    val tokenRequest = basicRequest
//      .post(uri"https://github.com/login/oauth/access_token?code=$authCode&grant_type=authorization_code")
//      .auth
//      .basic(username, password)
//      .header("accept", "application/json")
//    val authResponse = tokenRequest.response(asJson[GithubTokenResponse]).send(backend)
//    val accessToken = authResponse.body.map(_.accessToken).getOrElse(sys.error("Didn't get the token"))
//
//    val response = basicRequest.auth
//      .bearer(authCode)
//      .get(uri"https://adventofcode.com/2024/day/$day/input")
//      .send(backend)
//    response.body.foreach(_ => Files.writeString(inputFilePath, _))
//  }

  protected val testInput: Seq[String] =
    Using(Source.fromFile(s"./src/main/resources/test-input/$day.txt"))(_.getLines().toSeq).get
  protected val input: Seq[String] =
    Using(Source.fromFile(inputFileAddress))(_.getLines().toSeq).get
  protected val testCommonResult: CommonResult = calculateCommonResult(testInput)
  protected val commonResult: CommonResult = calculateCommonResult(input)

  def main(args: Array[String]): Unit = {
    val testResultPart1 = solvePart1(testInput, testCommonResult)
    println(s"Test result part1: $testResultPart1")
    val testResultPart2 = solvePart2(testInput, testCommonResult)
    println(s"Test result part2: $testResultPart2")

    val resultPart1 = solvePart1(input, commonResult)
    println(s"Result part1: $resultPart1")
    val resultPart2 = solvePart2(input, commonResult)
    println(s"Result part2: $resultPart2")

  }

object SolutionBase:
  given tokenResponseDecoder: Decoder[GithubTokenResponse] = deriveDecoder[GithubTokenResponse]
