<?php

/* PHP Formatter
 *
 * Copyright (c) 2013 Nathan Wong (http://nathan.ca)
 *
 * Licensed under the MIT licensed at http://nathan.ca/code/license
 *
 * Last updated January 1, 2013
 *
 */

class Formatter
{
	// helpers:
	private static function PrettyPrint($t)
	{
		if (count($t) == 3)
			echo token_name($t[0]) . "\t\t[" . $t[1] . ']';
		else
			echo $t[0];
		echo "\n";
	}
	
	// normalize the individual rows from token_get_all
	static function Tokenize($t)
	{
		return array($t[0], count($t) == 3 ? $t[1] : $t[0]);
	}
	// calculate the actual length of a string using a 4-space tab versus a 1-char tab
	static function SmartLen($str)
	{
		return strlen($str) + substr_count($str, "\t") * 3; // 4-space tab
	}
	
	// let's get started :)
	private static $linelen = 96;
	private $code;
	function __construct($code)
	{
		$this->code = $code;
	}
	
	// replace each token with the correctly formatted/spaced value
	private function formatCode($code)
	{
		// keep track of indentation and control state, abstracted through newline/blankline
		$tabs = 0;
		$brackets = 0;
		$lastcontrol = false;
		$lambda = false;
		$oneliner = false;
		$ternary = 0;
		$newline = function ($char, $before = false) use (&$tabs, &$oneliner) {
			return function ($requirews = '') use (&$tabs, &$oneliner, $char, $before) {
				if ($oneliner)
					return $requirews;
				return ($before ? "\n" . str_repeat("\t", max($tabs, 0)) : '') . $char . "\n" .
					str_repeat("\t", max($tabs, 0));
			};
		};
		$blankline = $newline('');
		
		// maintain a running stack of curly brackets to know how many control statements to
		// unindent by on closing brackets.
		$controls = array(0);
		$controlbrackets = array();
		$control = function ($keyword) use 
		(&$lastcontrol, &$controls, &$controlbrackets, &$brackets) {
			return function ($v, $t) use 
			(&$lastcontrol, &$controls, &$controlbrackets, &$lambda, &$brackets, $keyword) {
				$lastcontrol = $t;
				$lambda = false;
				$controls[count($controls) - 1]++;
				
				$controlbrackets[] = $brackets;
				
				return $keyword;
			};
		};
		$uncontrol = function () use (&$lastcontrol, &$controls, &$controlbrackets, &$tabs) {
			// unindent as many as needed
			$lastcontrol = false;
			
			while(count($controls) > 1 && $controls[count($controls) - 1] == 0)
				array_pop($controls); // get rid of empty states
			$tabs -= array_pop($controls);
			$controls[] = 0; // re-add the last frame on
			array_pop($controlbrackets);
		};
		
		// define what each token should be substituted with. $$ represents "leave as-is".
		// alternately a callback function can be specified to offer context-aware replacements
		$replacements = array(T_ABSTRACT => 'abstract ', T_AND_EQUAL => ' &= ', T_ARRAY =>
				'array',
			T_ARRAY_CAST => '(array) ', T_AS => ' as ', T_BAD_CHARACTER => '$$',
			T_BOOLEAN_AND => ' && ',
			T_BOOLEAN_OR => ' || ', T_BOOL_CAST => '(bool) ', T_BREAK => 'break', T_CASE =>
				'case ',
			T_CHARACTER => '$$', T_CLASS_C => '__CLASS__', T_CLONE => 'clone ',
			T_CONCAT_EQUAL => ' .= ',
			T_CONST => 'const ', T_CONSTANT_ENCAPSED_STRING => '$$', T_CONTINUE => 'continue',
			T_CURLY_OPEN => '$$',
			T_DEC => '--', T_DECLARE => 'declare', T_DEFAULT => 'defalt', T_DIR => '__DIR__',
			T_DIV_EQUAL => ' /= ',
			T_DNUMBER => ' $$ ', T_DOC_COMMENT => '$$', T_DOLLAR_OPEN_CURLY_BRACES => '$$',
			T_DOUBLE_ARROW => ' => ',
			T_DOUBLE_CAST => '(double) ', T_DOUBLE_COLON => '::', T_ECHO => 'echo ', T_EMPTY =>
				'empty',
			T_ENCAPSED_AND_WHITESPACE => '$$', T_ENDDECLARE => 'enddeclare', T_END_HEREDOC =>
				'$$',
			T_EVAL => '$$', T_EXIT => 'exit', T_EXTENDS => ' extends ', T_FILE => '__FILE__',
			T_FINAL => 'final ',
			T_FUNC_C => '__FUNCTION__', T_GLOBAL => 'global ', T_GOTO => 'goto ',
			T_HALT_COMPILER => '$$',
			T_IMPLEMENTS => ' implements ', T_INC => '++', T_INCLUDE => 'include ',
			T_INCLUDE_ONCE => 'include_once ',
			T_INSTANCEOF => ' instanceof ', T_INT_CAST => ' (int) ', T_INTERFACE =>
				'interface ',
			T_ISSET => 'isset', T_IS_EQUAL => ' == ', T_IS_GREATER_OR_EQUAL => ' >= ',
			T_IS_IDENTICAL => ' === ',
			T_IS_NOT_EQUAL => ' != ', T_IS_NOT_IDENTICAL => ' !== ', T_IS_SMALLER_OR_EQUAL =>
				' <= ',
			T_LINE => '__LINE__', T_LIST => 'list', T_LNUMBER => '$$', T_LOGICAL_AND =>
				' and ',
			T_LOGICAL_OR => ' or ', T_LOGICAL_XOR => ' xor ', T_METHOD_C => '__METHOD__',
			T_MINUS_EQUAL => ' -= ',
			T_MOD_EQUAL => ' %= ', T_MUL_EQUAL => ' *= ', T_NAMESPACE => 'namespace ',
			T_NS_C => '__NAMESPACE__',
			T_NS_SEPARATOR => '\\', T_NEW => 'new ', T_NUM_STRING => '$$', T_OBJECT_CAST =>
				'(object) ',
			T_OBJECT_OPERATOR => '->', T_OPEN_TAG_WITH_ECHO => '<?=', T_OR_EQUAL => ' |= ',
			T_PAAMAYIM_NEKUDOTAYIM => '::',
			T_PLUS_EQUAL => ' += ', T_PRINT => 'print', T_PRIVATE => 'private ', T_PUBLIC =>
				'public ',
			T_PROTECTED => 'protected ', T_REQUIRE => 'require ', T_REQUIRE_ONCE =>
				'require_once ',
			T_SL => ' << ', T_SL_EQUAL => ' <<= ', T_SR => ' >> ', T_SR_EQUAL => ' >>= ',
			T_START_HEREDOC => '$$',
			T_STATIC => 'static ', T_STRING_CAST => '(string) ', T_STRING_VARNAME => '$$',
			T_THROW => 'throw ',
			T_UNSET => 'unset', T_UNSET_CAST => '(unset) ', T_USE => ' use ', T_VAR => 'var ',
			T_VARIABLE => '$$',
			T_XOR_EQUAL => ' ^= ', '+' => ' + ', '-' => ' - ', '*' => ' * ', '/' => ' / ',
			'%' => ' % ',
			'.' => ' . ', '>' => ' > ', '<' => ' < ', '=' => ' = ', '!' => '!', '^' => ' ^ ',
			',' => ', ',
			T_STRING => function ($v, $t, $tokens, $i) {
				// class type-hinting
				if (isset($tokens[$i + 2]) && ($tokens[$i + 1][0] == T_VARIABLE || 
									($tokens[$i + 1][0] == T_WHITESPACE &&
									 $tokens[$i + 2][0] == T_VARIABLE)))
					return $v . ' ';
				
				// static scope
				return $v;
			}, T_COMMENT => function ($v) use ($blankline) {
				return rtrim($v) . (substr($v, - 2) == '*/' ||
						 substr($v, - 1) == "\n" ? $blankline() : '');
			}, T_RETURN => function ($v, $t, $tokens, $i) use (&$tabs) {
				if ($tokens[$i + 1][0] == ';')
					return 'return';
				
				return 'return ';
			}, T_IF => $control('if '), T_ELSE => function ($v, $t, $tokens, $i) use 
						(&$lastcontrol, $control, &$tabs, $blankline) {
				// if we're followed by an "if" (i.e. we're an else-if), don't handle it
				if ($tokens[$i + 1][0] == T_IF || 
							($tokens[$i + 1][0] == T_WHITESPACE && $tokens[$i + 2][0] == T_IF))
					return 'else ';
				
				// normal else, treat it like an immediate control statement
				$else = $control('else');
				$else($v, $t, $tokens, $i);
				
				// already "past" the condition brackets (so do the ')' handling here)
				$lastcontrol = T_ELSE + 200000;
				$tabs++;
				return 'else' . $blankline(' ');
			}, T_TRY => function ($v, $t, $tokens, $i) use 
					(&$lastcontrol, $control, &$tabs, $blankline) {
				
				// immediate control statement
				$try = $control('try');
				$try($v, $t, $tokens, $i);
				
				// already "past" the condition brackets (so do the ')' handling here)
				$lastcontrol = T_TRY + 200000;
				$tabs++;
				return 'try' . $blankline(' ');
			}, T_CATCH => $control('catch'), T_DO => function ($v, $t, $tokens, $i) use 
						(&$lastcontrol, &$control, &$tabs, $blankline) {
				// treat "do" just like "else"
				$do = $control('do');
				$do($v, $t, $tokens, $i);
				
				$lastcontrol = T_DO + 200000;
				$tabs++;
				
				return 'do ';
			}, T_WHILE => $control('while'), T_FOR => $control('for'), T_CLASS => $control(
						'class '),
				T_FUNCTION => function ($v, $t, $tokens, $i) use 
					($control, &$lambda, &$brackets, &$controlbrackets) {
				$func = $control('function ');
				$ret = $func($v, $t, $tokens, $i);
				
				// need to special-handle lambdas here. if there's anything preceeding this
				// function that's a dollar sign, then we need to leave the bracket on us.
				$lambda = false;
				for($tempi = $i; $tempi >= 0 && !$lambda; $tempi--)
				{
					list($tempt, $tempv) = Formatter::Tokenize($tokens[$tempi]);
					
					if (strpos($tempv, "\n") !== false)
						break;
					else if (strpos($tempv, "$") !== false || strpos($tempv, '(') !== false ||
						 strpos($tempv, '=') !== false || $tempt == T_RETURN)
						$lambda = true;
				}
				
				return $ret;
			}, T_FOREACH => $control('foreach'), '{' => function () use 
					(&$lastcontrol, &$controls, &$tabs, &$lambda, $blankline) {
				
				$pre = '';
				// one exception to using the control statement as the indenter is the class
				// declaration, where there aren't any normal brackets. so we do it here.
				if ($lastcontrol == T_CLASS)
				{
					$tabs++;
					$pre = $blankline();
				}
				else if ($lambda)
				{
					$tabs++; // normal control statement
					$pre = ' ';
					$lambda = false;
				}
				
				$lastcontrol =  - 1;
				$controls[] = 0; // new "stack" of control statements at the curly bracket
				return $pre . '{' . $blankline();
			}, '}' => function ($v, $t, $tokens, $i) use ($uncontrol, $blankline) {
				$uncontrol();
				
				// handle brackets in expressions versus in statements
				if (isset($tokens[$i + 1]) && ($tokens[$i + 1] == ';' ||
							 $tokens[$i + 1] == ',' || $tokens[$i + 1] == ')'))
				{
					$lambda = false;
					return '}';
				}
				return '}' . $blankline();
			}, '(' => function () use (&$brackets, &$tabs) {
				$tabs++;
				$brackets++;
				return '(';
			}, ')' => function ($v, $t, $tokens, $i) use (&$lastcontrol, &$brackets,
						&$controlbrackets, &$tabs, &$lambda, $blankline) {
				$tabs--;
				$brackets--;
				
				if ($brackets == end($controlbrackets) && $lastcontrol > 0 &&
					 $lastcontrol < 200000 && !$lambda)
				{
					$tabs++;
					$lastcontrol += 200000;
					
					// if we're followed by a comment before a newline, do not add a newline.
					// also, if we're followed by a semicolon, don't add a newline or space.
					if (isset($tokens[$i + 1]) && $tokens[$i + 1][0] == ';')
						return ')';
					else if ((isset($tokens[$i + 1]) && $tokens[$i + 1][0] == T_COMMENT) || 
								(isset($tokens[$i + 2]) && $tokens[$i + 1][0] ==
									T_WHITESPACE && strpos($tokens[$i + 1][1], "\n") ===
									false && $tokens[$i + 2][0] == T_COMMENT))
						return ') ';
					return ')' . $blankline();
				}
				
				return ')';
			}, ';' => function ($v, $t, $tokens, $i) use 
					(&$lastcontrol, $uncontrol, $blankline) {
				// if this is a one-line control statement, unindent
				if ($lastcontrol >= 200000)
					$uncontrol();
				
				// if we're followed by a comment before a newline, do not add a newline.
				if ((isset($tokens[$i + 1]) && $tokens[$i + 1][0] == T_COMMENT) || (isset(
								$tokens[$i + 2]) && $tokens[$i + 1][0] ==
								T_WHITESPACE && strpos($tokens[$i + 1][1], "\n") === false &&
							 $tokens[$i + 2][0] == T_COMMENT))
					return '; ';
				
				// for loops are the obvious exception where semicolons are allowed in-line
				if ($lastcontrol == T_FOR)
					return '; ';
				
				// otherwise, add a line after this statement
				return ';' . $blankline(' ');
			}, '?' => function () use (&$ternary) {
				$ternary++;
				return ' ? ';
			}, ':' => function () use (&$ternary) {
				if ($ternary > 0)
				{
					$ternary--;
					return ' : ';
				}
				// non-ternary, do not pad the colon (e.g. alt control syntax)
				return ': ';
			}, T_OPEN_TAG => function ($v, $t, $tokens, $i) use 
					(&$tabs, &$oneliner, $blankline) {
				// unfortunately, the open tag INCLUDES the trailing newline if there is one.
				// so we need to look ahead and maintain newlines appropriately
				$oneliner = false;
				if ($tokens[$i + 1][0] == T_WHITESPACE &&
					 strpos($tokens[$i + 1][1], "\n") !== false && strpos($v, "\n") !== false)
					return '<' . '?php' . $blankline() . $blankline();
				
				$oneliner = true;
				return '<' . '?php ';
			}, T_CLOSE_TAG => function ($v, $t, $tokens, $i) use (&$oneliner, $blankline) {
				// as with the open tag, the closing tag includes the trailing newline.
				if (strpos($v, "\n") !== false)
					return '?>' . $blankline("\n");
				
				return '?>';
			}, T_WHITESPACE => function ($v) use (&$tabs, $blankline) {
				
				// get rid of all whitespace except for double newlines, which we mark using
				// the (now-unused) carriage return and subsequently convert that to properly
				// indented lines. we will then re-add all standard whitespace that we actually
				// want directly, ensuring consistency
				return str_replace("\r", $blankline(), str_replace("\n", '', str_replace(
							"\n\n", "\r", str_replace(array("\t", ' ', "\r"), '',
								$v))));
			}, T_INLINE_HTML => '$$',
				// handled separately
			);
		
		// all set... let's go
		$tokens = token_get_all($this->code);
		//array_walk($tokens, array('Formatter', 'PrettyPrint'));
		$ret = '';
		$cleanup = function ($v, $token) {
			return str_replace("\r", '', $v);
		};
		foreach($tokens as $i => $t)
		{
			list($token, $val) = self::Tokenize($t);
			$val = $cleanup($val, $token);
			
			if (!isset($replacements[$token]))
				$ret .= $val;
			else
			{
				$transformation = $replacements[$token];
				$ret .= is_callable($transformation) ? $transformation($val, $token, $tokens,
						$i) : str_replace('$$', strval($val), $transformation);
			}
		}
		
		// since we (incorrectly, but conveniently) indent the curly brackets, we need to
		// remove a single preceeding tab
		$ret = str_replace(array("\t{", "\t}"), array('{', '}'), $ret);
		
		//
		return $ret;
	}
	// prevent lines from rolling over the $linelen specification
	private function breakLongLines($code)
	{
		// for debugging purposes, show asterisks across the top with the line length.
		//echo str_repeat('*', self::$linelen)."\n";
		$tokens = token_get_all($code);
		// when we hit an operator, we look ahead to see if we can make it to the next
		// operator without going over our limit. if we can't, we break after ourselves. if we
		// can, we instead jump to that point and keep going.
		//
		// we also need to maintain our indentation level when we break after oureslves.
		$final = array();
		$indent = ''; // amount of indentation on the current line
		$chars = 0; // chars since last newline
		$lastop =  - 1; // position in tokens of last operator
		$lastchars = 0; // chars since lastop
		$lastorgline = 0; // the last original (not added here) newline
		$opstack = array();
		$broken = false;
		
		for($tokeni = 0; $tokeni < count($tokens); $tokeni++)
		{
			list($token, $val) = self::Tokenize($tokens[$tokeni]);
			$final[] = $val;
			
			//
			if (($token == T_WHITESPACE && strpos($val, "\n") !== false) || 
						($token == T_COMMENT && substr($val, - 1) == "\n") || 
						($token == T_OPEN_TAG && substr($val, - 1) == "\n"))
			{
				// re-balance the previous line now that we have complete info. basically, we
				// just make certain operators "greedy" in that if we can keep them together
				// without adding more lines, we'll do it. (this is purely asthetic, but then
				// again, isn't all formatting?)
				$valid = function ($chunks, $maxlen) {
					$len = 0;
					foreach($chunks as $chunk)
					{
						if (strpos($chunk, "\n") !== false)
						{
							$len += Formatter::SmartLen(current(explode("\n", $chunk)));
							if ($len > $maxlen)
								return false;
							
							// restart
							$len = Formatter::SmartLen(end(explode("\n", $chunk)));
						}
						else
						{
							$len += Formatter::SmartLen($chunk);
							
							if ($len > $maxlen)
								return false;
						}
					}
					return true;
				};
				$findlines = function ($d) {
					return array_filter($d, function ($v) {
							return strpos($v, "\n") !== false;
						});
				};
				
				//
				$slice = array_slice($final, $lastorgline, count($final) - $lastorgline - 1);
				$breaksadded = $findlines($slice);
				$bslen = count($breaksadded);
				if ($bslen > 0 && $valid($slice, self::$linelen))
				{
					// we're going to try each of " (", "||", and "&&" for additional
					// breakage, stopping if we successfully change the string. basically, if
					// there's a bracket following a space, we re-bias the line against that.
					// failing that, however, as most if statements do, we'll try to do the same
					// thing with the next highest precedence operator, the ||, and then failing
					// that try again with && (again since most if statements are just &&)
					$changes = 0;
					$ops = array('(', '||', '&&');
					for($opsi = 0, $opslen = count($ops); !$changes &&
						 $opsi < $opslen; $opsi++)
					{
						// clone the slice for this iteration
						$ts = $slice;
						// only make one change at a time, to avoid index munging (this may
						// not be strictly necessary anymore since we modify instead of insert
						// now, but it's still handy to ensure consistency)
						do {
							$modified = false;
							
							// since we add newlines left-to-right, it makes sense to be
							// greedy from right-to-left, since that's what's likely to have
							// been forgotten. so, let's move the LAST newline to after the
							// preceeding operator.
							$bsa = array_keys(array_reverse($breaksadded, true));
							for($bsk = 0; !$modified && $bsk < $bslen; $bsk++)
							{
								$bsi = $bsa[$bsk];
								
								$orgline = $ts[$bsi];
								$ts[$bsi] = rtrim($orgline, "\n\t");
								$lineform = str_replace("\n\t", "\n", substr($orgline, strlen(
											$ts[$bsi])));
								if (empty($ts[$bsi]))
									$ts[$bsi] = ' ';
								
								for($tsi = $bsi - 1; !$modified && $tsi >= 0; $tsi--)
								{
									// bracket preceeded by space is the BEST place to break
									if ($ts[$tsi] == $ops[$opsi] &&
										 strlen(trim($ts[$tsi - 1])) == 0)
									{
										$before = ($ops[$opsi] == '(' ? 1 : 0);
										$ts[$tsi - $before] .= $lineform;
										
										if ($valid($ts, self::$linelen) &&
													 count($findlines($ts)) <= $bslen)
										{
											// instead of array_splice'ing, we'll just copy
											// what's changed
											if ($final[$lastorgline + $tsi - $before] !=
													$ts[$tsi - $before] ||
												 $final[$lastorgline + $bsi] != $ts[$bsi])
											{
												$final[$lastorgline + $tsi -
													$before] = $ts[$tsi - $before];
												$final[$lastorgline + $bsi] = $ts[$bsi];
												
												$modified = true;
												$changes++;
											}
										}
										
									}
								}
							}
						}
						while($modified);
					}
				}
				$lastorgline = count($final); // reset for next time now set up the NEXT
					// line: reset the indentation with the whitespace at the end of this line,
					// or the next whitespace (if we're actually a comment)
				$indent = end(explode("\n", $val));
				if (($tokeni + 1) < count($tokens) && $tokens[$tokeni + 1][0] == T_WHITESPACE)
					$indent .= $tokens[$tokeni + 1][1];
				$chars = $token == T_COMMENT ? 0 : self::SmartLen($indent);
				
				$lastchars = 0;
				$lastop =  - 1;
				$opstack = array();
			}
			else if ($token == T_INLINE_HTML)
			{
				$lastline = end(explode("\n", $val));
				$lastchars = $chars = self::SmartLen($lastline);
				$indent = preg_replace('/[^\t]/', '', $lastline);
			}
			else
			{
				$chars += self::SmartLen($val);
				$lastchars += self::SmartLen($val);
				
				if ($chars >= self::$linelen)
				{
					// go back to $lastop and add a newline in the placeholder after it
					if ($lastop >= 0)
					{
						$fullindent = $indent . str_repeat("\t", count($opstack));
						
						if (strlen(trim($final[$lastop])) == 0)
							$final[$lastop] = '';
						
						// if we're an "inline" bracket (i.e. a space before), put newline
						// first
						if (trim($final[$lastop]) == '(' &&
							 substr($final[$lastop - 1], - 1) == ' ')
							$final[$lastop] = "\n" . $fullindent . $final[$lastop];
						else
							$final[$lastop] .= "\n" . $fullindent;
						
						$lastop =  - 1;
						$chars = $lastchars + self::SmartLen($fullindent);
						$lastchars = self::SmartLen($fullindent);
						$broken = true;
					}
				}
				
				// so what we need to do is establish a stack of operators. when an opening
				// bracket is hit, we establish a new level, one indent deep. we undo this at
				// the corresponding closing bracket. commas are a bit stranger. we create a new
				// stack element with the comma, but it's actually closed by a comma or a
				// closing bracket (and reopened, obviously, if it's by a comma). this is
				// because the comma and brackets are actually all on one level. then, normal
				// operators like math operators or dots create a new TEMPORARY indent level.
				// all lines following these temp indent levels are indented again, but they die
				// at any other operator. moreso, if that delimiting operator is a comma, a
				// newline is forced, even if we're no where near the length max. this is
				// because the expression following the comma is not at the same indentation
				// level as the temp
				//
				if ($token == '(')
				{
					// only break on opening brackets that aren't immediately closed (i.e.
					// empty)
					if ($tokens[$tokeni + 1][0] != ')')
					{
						// fake a comma
						$opstack[] = array(',', $tokeni, 'FAKE(');
						$lastop = $tokeni;
						$broken = false;
					}
				}
				else if ($token == ')')
				{
					// only break on opening brackets that aren't immediately closed (i.e.
					// empty)
					if ($tokens[$tokeni - 1][0] != '(')
					{
						// go back to the last opening bracket
						while(count($opstack) > 0 && end(end($opstack)) != 'FAKE(')
							array_pop($opstack);
						array_pop($opstack);
					}
				}
				else if ($token == ',')
				{
					$lastop = ++$tokeni;
					$lastchars = 0;
					$final[] = ' ';
					$chars++; // space we added
					$break = false;
					// take off all indenting back to the previous comma. if we pass anything
					// else, then we need a newline.
					while(count($opstack) > 0 && current(end($opstack)) != ',')
						if (!in_array(current(array_pop($opstack)), array(',')) && $broken)
							$break = true;
					
					if ($break)
					{
						$fullindent = $indent . str_repeat("\t", count($opstack));
						
						if (strlen(trim($final[$lastop])) == 0)
							$final[$lastop] = '';
						$final[$lastop] .= "\n" . $fullindent;
						$lastop =  - 1;
						$chars = $lastchars + self::SmartLen($fullindent);
						$lastchars = self::SmartLen($fullindent);
						$broken = false;
					}
					// if we took everything out, add a comma
					if (count($opstack) == 0 || current(end($opstack)) == '(')
						$opstack[] = array($token, $tokeni);
				}
				else if (in_array($token, array('.', '?', ':', T_BOOLEAN_AND, T_BOOLEAN_OR,
							'*', '+', '-', '/', '%', '<', '>', T_IS_NOT_EQUAL, T_IS_EQUAL,
							T_IS_IDENTICAL, T_IS_NOT_IDENTICAL, T_IS_SMALLER_OR_EQUAL,
							T_IS_GREATER_OR_EQUAL, T_DOUBLE_ARROW)))
				{
					$lastop = ++$tokeni; // increment to eat the original space
					$lastchars = 0;
					$final[] = ' '; // placeholder
					$chars++; // the space we just added take off any other
					// non-comma (i.e. temp) operator indent and replace it
					if (count($opstack) > 0 && current(end($opstack)) != ',')
						array_pop($opstack);
					$opstack[] = array($token, $tokeni);
				}
			}
		}
		
		return implode('', $final);
	}
	
	// format comments to be within $linelen chars long
	private function formatComments($code)
	{
		// the rules here are tricky. first, normal single-line comments need to be broken at
		// the same $linelen as the code, UNLESS the comment starts with more than a single
		// space. if it starts with multiple spaces, then it should not combine with the
		// preceeding or succeeding lines, as this indicates intentionally formatted text (e.g.
		// a list). furthermore, blank comments should not be merged.
		//
		$wrap = function ($comment, $indentation, $len, $suffix = '') {
			// tack on the prefix length as spaces in front of the comment so that it only
			// applies to the first line, and then trim it back off once we've wordwrap'ed the
			// whole comment to the appropriate length.
			$comment = str_replace("\n//", '', $comment);
			return trim(wordwrap($comment, $len - Formatter::SmartLen($indentation) - 3, "\n" .
						$indentation . '// '), ' ') . $suffix;
		};
		
		//
		$tokens = token_get_all($code);
		$final = array();
		$comment = '';
		$commenti = 0;
		$prefix = $indentation = '';
		$len = self::$linelen;
		$breakwrap = function ($suffix = '') use 
		(&$final, &$commenti, &$comment, &$prefix, &$indentation, $len, $wrap) {
			$final[$commenti] = $wrap(str_repeat(' ', max(0, Formatter::SmartLen($prefix) -
				4)) . $comment,
				$indentation, $len, $suffix);
			$prefix = $indentation = $comment = '';
			$commenti = 0;
		};
		for($i = 0; $i < count($tokens); $i++)
		{
			list($token, $val) = self::Tokenize($tokens[$i]);
			
			if ($token == T_COMMENT && substr($val, 0, 3) == '// ' &&
						 !in_array(substr($val, 3, 1), array("\n", "\t", ' ')))
			{
				// OK, we're an eligible comment. tack it on, and track it if we're starting
				if (empty($comment))
				{
					$final[] = "// PLACEHOLDER\n";
					$commenti = count($final) - 1;
					
					// go back and find the appropriate indentation. if it wasn't immediate,
					// add a tab to it since it's carried-over.
					$prefix = $indentation = '';
					for($k = $i - 1; $k >= 0; $k--)
					{
						list($kt, $kv) = self::Tokenize($tokens[$k]);
						// if it's whitespace containing a new line, use its tabs.
						if ($kt == T_WHITESPACE && strpos($kv, "\n") !== false)
						{
							$indentation = end(explode("\n", $kv));
							if ($k != $i - 1)
								$indentation .= "\t";
							break;
						}
						// if it's a newline-holding other token, use the next token's tabs
						else if ((in_array($kt, array(T_COMMENT, T_WHITESPACE, T_OPEN_TAG,
										T_CLOSE_TAG)) && strpos($kv, "\n") !== false))
						{
							list($kt1, $kv1) = self::Tokenize($tokens[$k + 1]);
							if ($kt1 == T_WHITESPACE)
							{
								$indentation = $kv1;
								break;
							}
						}
						
						if ($kt != T_COMMENT)
							$prefix .= $kv; // backwards, but we only care about length
					}
				}
				else if (current($tokens[$i - 1]) == T_WHITESPACE)
					array_pop($final); // remove the whitespace between comments.
				$comment .= $val;
				$val = ''; // mark val as "handled" so it doesn't get added
			}
			else if (!empty($comment) && ($token == T_WHITESPACE && (strpos($val, "\n\n") !==
									false) || 
				($i > 0 && strpos($val, "\n") !== false &&
									substr(end(self::Tokenize($tokens[$i - 1])), - 1) == "\n")))
			{
				// double newline: break.
				$breakwrap("");
			}
			else if ($token != T_WHITESPACE && !empty($comment))
			{
				// not a comment (or not an eligible comment), so we have one to process.
				// place it accordingly.
				$breakwrap();
			}
			
			//
			if (strlen($val) > 0)
				$final[] = $val;
		}
		
		return implode('', $final);
	}
	
	// format HTML to be indented logically
	function formatHTML($code)
	{
		// formatting HTML is considerably more permissive. lines won't be broken or
		// reformatted, just indented correctly, and excessive whitespace may be removed.
		//
		// the indentation rule is simple: each line with an opening tag represents ONE
		// indentation level, even if 5 tags were opened. corresponding closing tags, however
		// will be put together on one line for the closing tag to match up with the opening.
		//
		$ret = '';
		$opentag = null; // current open tag
		$lws = true;
		$script = ''; // store javascript here and then format it separately
		$comment = false;
		$tags = array(array()); // stack of open tag-lines
		$closed = 0; // # of tags closed on this line
		$lastnewline = 0; // index in $ret
		$unindent = false;
		$forced = array();
		
		$newline = function () use (&$tags) {
			return "\n" . str_repeat("\t", count(array_filter($tags)));
		};
		
		$cur = &$ret;
		$tokens = token_get_all($code);
		foreach($tokens as $token)
		{
			list($token, $val) = self::Tokenize($token);
			
			if ($token == T_INLINE_HTML || $token == T_CLOSE_TAG)
			{
				if ($token == T_CLOSE_TAG)
				{
					// eat the closing tag and process the newline as HTML.
					$cur .= '?>';
					$val = substr($val, 2);
				}
				for($i = 0, $len = strlen($val); $i < $len; $i++)
				{
					$ch = $val[$i];
					
					// handle whitepsace
					if ($ch == "\n")
					{
						// ignore newlines in tags
						if (is_null($opentag))
						{
							// everything following the newline is leading whitespace
							$lws = true;
							
							// add a new tagline on the newline if we had tags before. if we
							// closed a tag but still have open tags, clear them now.
							if (count(end($tags)) > 0)
							{
								if ($closed > 0)
								{
									foreach(end($tags) as $tag)
									{
										$ret .= '</' . $tag . '>';
										$forced[] = $tag;
										$unindent = true;
									}
									// replace the whole tagline with a new empty one
									$tags[count($tags) - 1] = array();
								}
								else
								{
									// tack on a new layer
									$tags[count($tags)] = array();
								}
							}
							if ($unindent)
							{
								// unindent the previous line (the line that's ending) by the
								// number of tags we closed.
								$ret = substr($ret, 0, $lastnewline + 1) . substr($ret,
										$lastnewline + 2);
							}
							
							// and add a new line and mark it
							$lastnewline = strlen($ret);
							if (!empty($script))
								$script .= $newline();
							else
								$ret .= $newline();
							$closed = 0;
							$unindent = false;
						}
					}
					else if ($ch == ' ')
					{
						// allow all non-leading spaces
						if (!$lws)
							$cur .= ' ';
					}
					else if ($ch == "\t")
					{
						// allow "content" tabs
						if (!$lws)
							$cur .= "\t";
					}
					else if ($ch == "\r")
						; // just strip carriage-returns flat out
					else
					{
						$lws = false;
						
						// handle comments
						if ($ch == '!' && !is_null($opentag) && empty($opentag))
						{
							$cur = &$ret;
							$cur .= '<!';
							$comment = 1;
							$opentag = null;
						}
						else if ($ch == '<' && $comment)
						{
							$comment++;
							$cur .= $ch;
						}
						else if ($ch == '>' && $comment)
						{
							$comment--;
							$cur .= $ch;
							if ($comment == 1)
							{
								$comment = false;
							}
						}
						// handle tags
						else if ($ch == '<')
						{
							$opentag = '';
							$cur = &$opentag;
						}
						else if ($ch == '>' && !is_null($opentag))
						{
							$fulltag = $opentag;
							
							if (substr((trim($fulltag)), - 1) == '/')
							{
								// we don't really have to do much of anything for
								// one-liners
								$cur = &$ret;
								$ret .= '<' . $fulltag . '>';
								$opentag = null;
							}
							else
							{
								$tag = current(explode(' ', $fulltag));
								$opentag = null;
								$cur = &$ret;
								
								// handle closing tags first
								if (strlen($tag) > 0 && $tag[0] == '/')
								{
									$tag = substr($tag, 1);
									
									// in an IDEAL world, all tags would have perfectly
									// matched closing tags; obviously this isn't always the
									// case. so we look through the current open tags, and if
									// we're the last opened tag, great! pop it off the stack,
									// close it, case closed.
									//
									// if the tag ISN'T the
									// last one, though, we need to close everything in between
									// right here, right now before closing the tag that's
									// actually being closed, and we have to mark it as having
									// been force-closed. when a force-closed tag is matched, it
									// should be popped off without closing.
									//
									// additionally, on
									// newlines the rest of the tagline will be closed and the
									// closing line will be unindented accordingly. more
									// information on that up at the \n handler.
									$already = array_search($tag, $forced);
									if ($already !== false)
										unset($forced[$already]);
									else
									{
										// first of all, remove the blank one if it exists.
										if (!count(end($tags)))
										{
											array_pop($tags);
											$unindent = true;
										}
										
										// if we're the correct tag, we're good to go
										$last = end(end($tags));
										if ($last == $tag)
										{
											if ($tag == 'script' && !empty($script))
											{
												$js = $this->formatJavascript($script);
												$ret .= str_replace("\n", $newline(), $js) .
													$newline();
												$script = '';
												
												// because of the way we tack this on, we get
												// some oddities with unindenting. let's just
												// handle that here for brevity.
												$unindent = false;
												if (substr($ret, - 1) == "\t")
													$ret = substr($ret, 0, - 1);
											}
											$ret .= '<' . $fulltag . '>';
											array_pop($tags[count($tags) - 1]);
											$closed++;
										}
										else
										{
											// otherwise, go through and close them all
											$strip = $missing = array();
											$gotit = false;
											for($tli = count($tags) - 1; !$gotit &&
												 $tli >= 0; $tli--)
											{
												for($ti = count($tags[$tli]) - 1; !$gotit &&
													 $ti >= 0; $ti--)
												{
													if ($tags[$tli][$ti] == $tag)
														$gotit = true;
													
													$missing[] = $tags[$tli][$ti];
													$strip[] = array($tli, $ti);
												}
											}
											if ($gotit)
											{
												foreach($strip as $taginfo)
												{
													unset($tags[$taginfo[0]][$taginfo[1]]);
													if (empty($tags[$taginfo[0]]))
														unset($tags[$taginfo[0]]);
												}
												
												$ret .= '</' . implode('></', $missing) . '>';
											}
											else
											{
												// do nothing: just strip the closing tag
												// that closes a mismatched tag
											}
										}
									}
								}
								else
								{
									$ret .= '<' . $fulltag . '>';
									if ($tag == 'script')
										$cur = &$script;
									$tags[count($tags) - 1][] = $tag;
									$closed--;
								}
							}
						}
						else
						{
							// pass-through to current active buffer
							$cur .= $ch;
						}
					}
				}
			}
			else
			{
				// tack on all non-inline HTML tokens as-is to whichever buffer is currently
				// active (either the return val or the current open tag)
				$cur .= $val;
			}
		}
		
		return $ret;
	}
	
	//
	function formatJavascript($script)
	{
		// if js beautifier is available, let's use it.
		if (file_exists('jsbeautifier.php'))
		{
			require_once ('jsbeautifier.php');
			$jsb = new JSBeautifier();
			$opts = new BeautifierOptions();
			
			$opts->indent_with_tabs = true;
			
			$script = $jsb->beautify($script, $opts);
		}
		return $script;
	}
	
	// format all
	private $formatted = null;
	function format()
	{
		if (is_null($this->formatted))
		{
			// PHASE 1: run through the tokens, making the appropriate replacements
			$this->formatted = $this->formatCode($this->code);
			
			// PHASE 2: cut long lines at $longlen characters to prevent line wrapping
			$this->formatted = $this->breakLongLines($this->formatted);
			
			// PHASE 3: format comments to not rollover the line length
			$this->formatted = $this->formatComments($this->formatted);
			
			// PHASE 4: format HTML to be correctly indented
			$this->formatted = $this->formatHTML($this->formatted);
		}
		
		return $this->formatted;
	}
	
	// Format the code in the specified file (a handy static function to kick things off)
	static function FormatFile($file)
	{
		$f = new Formatter(file_get_contents($file));
		
		return $f->format();
	}
}

// run it as a command line tool
$argc = $_SERVER['argc'];
$argv = $_SERVER['argv'];

if ($argc < 2)
{
	printf("Usage: %s input file [output file]\n" . "Input file can be '-' for stdin, " .
			"and output can be '_' for same as input.\n\n",
		$argv[0]);
	exit(0);
}

$out = Formatter::FormatFile($argv[1] == '-' ? 'php://stdin' : $argv[1]);
if ($argc > 2)
	file_put_contents($argv[2] = '_' ? $argv[1] : $argv[2], $out);
else
	echo $out;
