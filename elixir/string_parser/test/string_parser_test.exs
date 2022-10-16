defmodule StringParserTest do
  use ExUnit.Case
  doctest StringParser

  test "greets the world" do
    assert StringParser.hello() == :world
  end
end
