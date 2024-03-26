-module(my_html_generator).
-export([generate_html/0]).

generate_html() ->
    {ok,_} = erlydtl:compile("priv/templates/example_template.dtl", my_template),
    Data = [{title, "Test Page"}, {heading, "Hello, World!"}, {message, "This is a test of erlydtl."}],
    {ok, RenderedHtml} = my_template:render(Data),
    RenderedHtml.
