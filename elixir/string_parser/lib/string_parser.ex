defmodule StringParser do
  @moduledoc """
  Documentation for `StringParser`.
  """

  def parse(string) when is_binary(string) do
    string
    |> Code.string_to_quoted!()
    |> parseAST()
  end

  defp parseAST(term) when is_nil(term), do: term
  defp parseAST(term) when is_atom(term), do: term
  defp parseAST(term) when is_integer(term), do: term
  defp parseAST(term) when is_float(term), do: term
  defp parseAST(term) when is_binary(term), do: term
  defp parseAST(term) when is_list(term), do: Enum.map(term, &parseAST/1)

  defp parseAST({string_as_atom, _metadata, nil}) when is_atom(string_as_atom) do
    Atom.to_string(string_as_atom)
  end

  defp parseAST({a, b}), do: {parseAST(a), parseAST(b)}

  defp parseAST({:{}, _metadata, elements}) do
    elements
    |> Enum.map(&parseAST/1)
    |> List.to_tuple()
  end

  defp parseAST({:%{}, _metadata, elements}) do
    elements
    |> Enum.map(&parseAST/1)
    |> Enum.into(%{})
  end
end
