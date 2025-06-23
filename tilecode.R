# shared tile

logos <- fs::dir_ls(here::here("images/hex/"))



urlss <- c(
"https://codeium.com/",
"https://knowusuboaky.github.io/LLMAgentR/",
"https://posit-dev.github.io/acquaint/",
"https://github.com/heurekalabsco/axolotr",
"https://posit-dev.github.io/btw/",
"https://simonpcouch.github.io/buggy/",
"https://knowusuboaky.github.io/chatLLM/",
"https://mlverse.github.io/chattr/",
"https://simonpcouch.github.io/chores/",
"https://www.continue.dev/",
"https://github.com/features/copilot",
"https://github.com/cornball-ai/diffuseR",
"http://elmer.tidyverse.org/",
"https://jhk0530.github.io/gemini.R/",
"https://github.com/frankiethull/ggpal2",
"https://michelnivard.github.io/gptstudio/",
"https://gabrielkaiserqfin.github.io/groqR",
"https://dylanpieper.github.io/hellmer/",
"https://github.com/frankiethull/kuzco",
"https://github.com/simonpcouch/gander/",
"https://mlverse.github.io/mall/",
"https://mcpr.opifex.org/",
"https://mlverse.github.io/lang/",
"https://hauselin.github.io/ollama-r/",
"https://docs.ropensci.org/pangoling/",
"https://github.com/GabrielKaiserQFin/PerplexR/",
"https://tidyverse.github.io/ragnar/",
"https://www.ekotov.pro/rdocdump/",
"https://jbgruber.github.io/rollama/",
"https://r-pkg.thecoatlessprofessor.com/searcher/",
"https://tidychatmodels.albert-rapp.de/",
"https://edubruell.github.io/tidyllm/",
 "https://vitals.tidyverse.org/"
)

names(logos) <- urlss

# shuffle logos vector
samplelogos <- sample(logos)


hexsession::make_tile(local_images = samplelogos, local_urls = names(samplelogos),dark_mode = F)
