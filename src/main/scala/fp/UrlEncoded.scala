package fp

import java.util.regex.Pattern
import eu.timepit.refined.api.Validate

sealed abstract class UrlEncoded
object UrlEncoded {
  private[this] val valid: Pattern =
  	Pattern.compile("\\A(\\p{Alnum}++|[-.*_+=&]++|%\\p{XDigit}{2})*\\z")

  implicit def urlValidate: Validate.Plain[String, UrlEncoded] =
  	Validate.fromPredicate(
  		s => valid.matcher(s).find(),
  		identity,
  		new UrlEncoded {}
  	)
}
