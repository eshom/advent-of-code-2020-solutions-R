library(magrittr)

input   <- readLines("input.txt") %>% as.numeric()

# Find two entries that sum to 2020
sum2020 <- sapply(input, function(x, input) (x + input) == 2020, input = input)
rownames(sum2020) <- input
colnames(sum2020) <- input
which(sum2020, arr.ind = TRUE) %>% rownames() %>% as.numeric() %>% prod()

# Find three entries that sum to 2020
inputcomb <- combn(input, 3)
threesum2020 <- collapse::dapply(inputcomb, function(x) sum(x) == 2020)
collapse::ss(inputcomb, 1:3, threesum2020) %>% prod()
