FROM codesimple/elm:0.18 as elm

RUN mkdir /korv
WORKDIR /korv

COPY App.elm /korv/
COPY elm-package.json /korv/
COPY inputs /korv/inputs
COPY AdventOfCode /korv/AdventOfCode
COPY Puzzles /korv/Puzzles
RUN elm make --yes App.elm


FROM nginx
COPY --from=elm /korv/index.html /usr/share/nginx/html/index.html
COPY --from=elm /korv/inputs /usr/share/nginx/html/inputs