defmodule StringParser do
  @moduledoc """
  Documentation for `StringParser`.
  """

  def parse(string) when is_binary(string) do
    string
    |> Code.string_to_quoted!()
    |> parseAST()
  end

  def parseAST(term) when is_nil(term), do: term
  def parseAST(term) when is_atom(term), do: term
  def parseAST(term) when is_integer(term), do: term
  def parseAST(term) when is_float(term), do: term
  def parseAST(term) when is_binary(term), do: term
  # def parseAST(term) when is_list(term), do: Enum.map(term, &parseAST/1)

  def parseAST({string_as_atom, _metadata, nil}) when is_atom(string_as_atom) do
    Atom.to_string(string_as_atom)
  end

  def parseAST({a, b}), do: {parseAST(a), parseAST(b)}

  def parseAST({:{}, _metadata, elements}) do
    elements
    |> Enum.map(&parseAST/1)
    |> List.to_tuple()
  end

  def parseAST({:%{}, _metadata, elements}) do
    elements
    |> Enum.map(&parseAST/1)
    |> Enum.into(%{})
  end

  def parseAST([]) do
    []
  end

  # Proper list
  def parseAST([x | []]) do
    [parseAST(x) | []]
  end

  def parseAST([x, {:|, _metadata, [a, b]}]) do
    [parseAST(x) | [parseAST(a) | parseAST(b)]]
  end

  # Improper list ending
  def parseAST({:|, _metadata, [a, b]}) do
    [parseAST(a) | parseAST(b)]
  end

  def parseAST([x | y]) do
    [parseAST(x) | parseAST(y)]
  end
end

# Todo: Update this script to call into the proper Mix project module. As of right now,
# this is not working correctly, so the module is repeated above (may have slight
# differences, so that should also be checked).

System.argv()
# |> IO.inspect()
|> List.first()
# |> Enum.join(",")
|> StringParser.parse()
# |> IO.inspect()
|> :erlang.term_to_binary()
|> IO.inspect()
