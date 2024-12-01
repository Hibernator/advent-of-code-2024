package ch.hibernator.adventofcode

case class GithubTokenResponse(accessToken: String, scope: String, tokenType: String, refreshToken: Option[String])
