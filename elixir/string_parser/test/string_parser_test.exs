defmodule StringParserTest do
  use ExUnit.Case
  import StringParser

  doctest StringParser

  test "parsing a string" do
    assert parse("testing") == "testing"
  end

  test "parsing atoms" do
    assert parse(":atom") == :atom
    assert parse(":asdf1") == :asdf1
  end
end
