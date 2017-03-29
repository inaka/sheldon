![](http://i.giphy.com/M3EZtLUDLsYP6.gif)

# Sheldon

Very Simple Erlang Spell Checker.

__Note:__ `Sheldon` also suggests correct words when some word is misspelled. That functionality was highly inspired by the Elixir project [spell_check](https://github.com/visar/spell_check).


## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/sheldon/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Installation

1. Clone the repo
2. `rebar3 compile`

## Usage

### Erlang Shell

First of all Sheldon is an application and it needs to be started. You can use `rebar3 shell` to set the necessary paths, then use `sheldon:start/0` or `application:ensure_all_started(sheldon)` in order to start Sheldon but if you are using Sheldon as a dependency you can let OTP starts it from your_app.app file too.

Sheldon only has two main methods, `sheldon:check/1` and `sheldon:check/2`. As a user, you just need to use those.

```erlang
1> sheldon:check("I want to check this correct text").
ok
2> sheldon:check("I want to check this misspeled text").
#{bazinga => <<"That's no reason to cry. One cries because one is sad. For example, I cry because others are stupid, and that ma"...>>,
  misspelled_words => [#{candidates => ["misspeed","misspelled"],
     line_number => 1,
     word => "misspeled"}]}
```

## Configuration

`sheldon:check/2` works like `sheldon:check/1` but it accepts a Configuration parameter.
With this Conf parameter we can apply some rules to the text we want to check. Those rules are ignore words, ignore patterns and ignore blocks.

This is the format (see [sheldon_config.erl](https://github.com/inaka/sheldon/blob/master/src/sheldon_config.erl)), no key is required:

```erlang
#{ ignore_words    => [string()]
 , ignore_patterns => [regex()]
 , ignore_blocks   => [ignore_block()]
 , adapters        => [adapter()]
 }.
```
Then, if we call the previous `sheldon:check/1` but with configuration we can skip the error

```erlang
3> sheldon:check("I want to check this misspeled text", #{ignore_words => ["misspeled"]}).
ok
```

## Adapters

Sometimes we have to check the spelling of formatted text but `sheldon` handles it as a plain text so we will face problems with that.
One example is `markdown` files, if we try to check them `sheldon` will complain about things like '##' or '\*something\*'.
For these cases `sheldon` provides `adapters`. An adapter is an Erlang module with an `adapt/1` function which will receive a line in `binary()` format and returns that line transformed.
For example, `sheldon` provides [markdown_adapter](https://github.com/inaka/sheldon/blob/master/src/adapter/markdown_adapter.erl) which converts from `markdown` to plain text.

In order to use them we only have to declare them in the config file:

```erlang
#{adapters => [markdown_adapter]}.
```

You can create your own adapter which fits your requirements, you only need to respect the `adapt/1` function signature.

```erlang
-spec adapt(binary()) -> iodata().
adapt(Line) ->
  ...
```

You can add all the adapters you want and they will be executed in order.

## Examples
Check [this](examples/README.md) out.

## Results

`sheldon:check/1` and `sheldon:check/2` have the same result type, you can see [sheldon_result.erl](https://github.com/inaka/sheldon/blob/master/src/sheldon_result.erl). Sheldon will return the `ok` atom if the check went well else it'll return
```erlang
    #{ misspelled_words := [misspelled_word()]
     , bazinga          := string()
     }.
```

## Dependencies

- Erlang/OTP 19+ in order to use it.
- Erlang/OTP 19.0.2+ in order to test it.
- Elixir "~> 1.4"
