# Checking Files
In order to check files, first we have to read them. In order to do that we are going to create a simple helper function:
```erlang
-spec check_file(string()) -> sheldon_result:result().
check_file(Path) ->
  {ok, Bin} = file:read_file(Path),
  sheldon:check(Bin).
```
Now we can check some files stored on disk. For example:
```erlang
1> example:check_file("files/romeo_juliet.txt").
#{bazinga => "I have never said that you are not good at what you do. It's just that what you do is not worth doing.",
  misspelled_words => [#{line_number => 7,word => "+"},
   #{line_number => 7,word => "MTV-inspired"},
   #{line_number => 7,word => "Luhrmann"},
   #{line_number => 7,word => "Baz"},
   #{line_number => 7,word => "Zeffirelli"},
   #{line_number => 7,word => "Cukor"},
   #{line_number => 7,word => "21st"},
   #{line_number => 7,word => "20th"},
   #{line_number => 7,word => "Gielgud"},
   #{line_number => 7,word => "Cushman"},
   #{line_number => 7,word => "19th"},
   #{line_number => 7,word => "Julie"},
   #{line_number => 7,word => "und"},
   #{line_number => 7,word => "Georg"},
   #{line_number => 7,word => "18th-century"},
   #{line_number => 7,word => "Davenant"},
   #{line_number => 5,word => "sub-plots"},
   #{line_number => 3,word => "Mercutio"},
   #{line_number => 3,word => "Brooke"},
   #{line_number => 3,word => "Romeus"}]}
 ```
 We see some names and words which are actually understood but there aren't on our dictionary. We can skip those improving a little our helper function including a config map as a parameter:
 ```erlang
 -spec check_file(string(), sheldon_config:config()) -> sheldon_result:result().
 check_file(Path, Config) ->
  {ok, Bin} = file:read_file(Path),
  sheldon:check(Bin, Config).
 ```
 try again
 ```erlang
1> Config = #{ignore_words => ["MTV-inspired", "Luhrmann", "Gielgud", "Cushman", "18th-century", "sub-plots"]}.
#{ignore_words => ["MTV-inspired","Luhrmann","Gielgud","Cushman",
   "18th-century","sub-plots"]}
2> example:check_file("files/romeo_juliet.txt", Config).
#{bazinga => "Too bad Leonard",
  misspelled_words => [#{line_number => 7,word => "+"},
   #{line_number => 7,word => "Baz"},
   #{line_number => 7,word => "Zeffirelli"},
   #{line_number => 7,word => "Cukor"},
   #{line_number => 7,word => "21st"},
   #{line_number => 7,word => "20th"},
   #{line_number => 7,word => "19th"},
   #{line_number => 7,word => "Julie"},
   #{line_number => 7,word => "und"},
   #{line_number => 7,word => "Georg"},
   #{line_number => 7,word => "Davenant"},
   #{line_number => 3,word => "Mercutio"},
   #{line_number => 3,word => "Brooke"},
   #{line_number => 3,word => "Romeus"}]}
 ```
Pretty easy with text files, right? But what if we try to check the current README file? lets try and see what happens...
```erlang
6> example:check_file("README.md").
#{bazinga => "Too bad Leonard",
  misspelled_words => [#{line_number => 63,word => "README"},
   #{line_number => 61,word => "=>"},
   #{line_number => 61,word => "=>"},
   #{line_number => 61,word => "#"},
   #{line_number => 60,word => "=>"},
   #{line_number => 60,word => "=>"},
   #{line_number => 60,word => "#"},
   #{line_number => 59,word => "=>"},
   #{line_number => 59,word => "=>"},
   #{line_number => 59,word => "#"},
   #{line_number => 58,word => "=>"},
   #{line_number => 58,word => "=>"},
   #{line_number => 58,word => "#"},
   #{line_number => 57,word => "=>"},
   #{line_number => 57,word => "=>"},
   #{line_number => 57,word => "#"},
   #{line_number => 56,word => "=>"},
   #{line_number => 56,word => "=>"},
   #{line_number => 56,word => "#"},
   #{line_number => 55,word => "=>"},
   #{line_number => 55,word => "=>"},
   #{line_number => 55,word => "#"},
   #{line_number => 54,word => "=>"},
   #{line_number => 54,word => "=>"},
   #{line_number => 54,word => [...]},
   #{line_number => 53,...},
   #{...}|...]}
 ```
 It doesn't look good, does it? 'sheldon' is complaining about the erlang code (obviously, no dictionary has Erlang terms...) what can we do?
 # Using Pattern blocks
 In order to fix our problem with Erlang code `sheldon` allows us to escape blocks of text using regex expressions for start/end blocks, lets try. If you see the raw version of this README, we have to escape blocks starting by "\`\`\`erlang" and finishing by "\`\`\`""
```erlang
8> Config = #{ignore_blocks => [#{open => "^```erlang", close => "```"}]}.
#{ignore_blocks => [#{close => "```",open => "^```erlang"}]}
9> example:check_file("README.md", Config).
#{bazinga => "The only way you'd be able to make a contribution to science is if they resume sending chimps into space.",
  misspelled_words => [#{line_number => 97,word => "README"},
   #{line_number => 97,word => "start/end"},
   #{line_number => 97,word => "regex"},
   #{line_number => 97,word => "Erlang"},
   #{line_number => 96,word => "#"},
   #{line_number => 95,word => "Erlang"},
   #{line_number => 95,word => "erlang"},
   #{line_number => 63,word => "README"},
   #{line_number => 34,word => "config"},
   #{line_number => 1,word => "#"}]}
```
Much better, right?
