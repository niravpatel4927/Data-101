market <- read.csv("Minimarket.csv")
head(market)

table(market$BREAD, market$BUTTER)
# table will show a 2x2 box
# left side has o and 1, which will represent 0 and 1 for 1st parameter, in this case
# 0 = no bread, and 1 = bread
# top side will represent no butter and butter
# the distribution of the 2x2 box starts by going down the left side, then moving 
# across the row
# so for this table it shows that 2412 people did not buy bread AND didn't buy butter
# 2419 people bought bread but did not buy butter etc.



# null hypothesis: bread doesn't impact the sale of butter
# alternative hypothesis: bread impacts the sale of butter

no_bread = subset(market, market$BREAD == 0)
yes_bread = subset(market, market$BREAD == 1)

no_bread_butter = no_bread$BUTTER
bread_butter = yes_bread$BUTTER

mean_no_bread_butter = mean(no_bread_butter)
mean_bread_butter = mean(bread_butter)
mean_no_bread_butter - mean_bread_butter
  
PermutationTestSecond::Permutation(market, "BREAD", "BUTTER", 10000, "0", "1")

# b/c the permutation returns a value >5%, we can say that bread does not impact the sale of butter


head(market)

table(market$BREAD, market$COOKIES)
# null hypothesis: bread doesn't impact sale of cookies
# alternative hypothesis: bread impacts the sale of cookies

no_bread_cookies = no_bread$COOKIES
bread_cookies = yes_bread$COOKIES

mean_no_bread_cookies = mean(no_bread_cookies)
mean_bread_cookies = mean(bread_cookies)
mean_no_bread_cookies - mean_bread_cookies

PermutationTestSecond::Permutation(market, "BREAD", "COOKIES", 10000, "1", "0")

# null hypothesis: bread doesn't impact the sale of coffee
# alternative hypothesis: bread does impact the sale of coffee

no_bread_coffee = no_bread$COFFEE
bread_coffee = yes_bread$COFFEE

mean(no_bread_coffee) - mean(bread_coffee)

PermutationTestSecond::Permutation(market, "BREAD", "COFFEE", 10000, "0", "1")

# null hypothesis: bread doesn't impact the sale of tea
# alternative hypothesis: bread affects sale of tea

table(market$BREAD, market$TEA)
no_bread_tea = no_bread$TEA
bread_tea = yes_bread$TEA

mean_no_bread_tea = mean(no_bread_tea)
mean_bread_tea = mean(bread_tea)
mean_no_bread_tea - mean_bread_tea

PermutationTestSecond::Permutation(market, "BREAD", "TEA", 10000, "1", "0")
# ya this one was completely off

table(market$BUTTER, market$COOKIES)
#null hypothesis: butter doesn't impact the sale of cookies
# alternative hypothesis: butter impacts the sale of cookies

no_butter = subset(market, market$BUTTER == 0)
yes_butter = subset(market, market$BUTTER == 1)

no_butter_cookies = no_butter$COOKIES
butter_cookies = yes_butter$COOKIES

mean_no_butter_cookies = mean(no_butter_cookies)
mean_butter_cookies = mean(butter_cookies)
mean_no_butter_cookies - mean_butter_cookies

PermutationTestSecond::Permutation(market, "BUTTER", "COOKIES", 10000, "0", "1")

# null hypothesis: butter doesn't impact the sale of coffee
# alternative hypothesis: butter impacts the sale of coffee

no_butter_coffee = no_butter$COFFEE
butter_coffee = yes_butter$COFFEE

mean(no_butter_coffee) - mean(butter_coffee)

PermutationTestSecond::Permutation(market, "BUTTER", "COFFEE", 10000, "0", "1")

# null hypothesis: butter doesn't impact the sale of tea
# alternative hypothesis: butter impacts the sale of tea

no_butter_tea = no_butter$TEA
butter_tea = yes_butter$TEA

mean(no_butter_tea) - mean(butter_tea)

PermutationTestSecond::Permutation(market, "BUTTER", "TEA", 10000, "1", "0")

# null hypothesis: cookies have no impact on coffee sales
# alternative hypothesis: cookies have a impact on coffee sales
table(market$COOKIES, market$COFFEE)
no_cookies = subset(market, market$COOKIES == 0)
yes_cookies = subset(market, market$COOKIES == 1)

no_cookies_coffee = no_cookies$COFFEE
cookies_coffee = yes_cookies$COFFEE

mean_no_cookies_coffee = mean(no_cookies_coffee)
mean_cookies_coffee = mean(cookies_coffee)
mean_no_cookies_coffee
mean_cookies_coffee

PermutationTestSecond::Permutation(market, "COOKIES", "COFFEE", 10000, "1", "0")
# p-value = 0.4484; failed to reject null

# null hypothesis: cookies don't impact the sale of tea
# alternative hypothesis: cookies impact the sale of tea

no_cookies = subset(market, market$COOKIES == 0)
yes_cookies = subset(market, market$COOKIES == 1)

no_cookies_tea = no_cookies$TEA
cookies_tea = yes_cookies$TEA

mean_no_cookies_tea = mean(no_cookies_tea)
mean_cookies_tea = mean(cookies_tea)

PermutationTestSecond::Permutation(market, "COOKIES", "TEA", 10000, "0", "1")
# p value was 0.0049, reject null

# null hypothesis: coffee has no impact on tea sales
# alternative hypothesis: coffee has an impact on tea sales

no_coffee = subset(market, market$COFFEE == 0)
yes_coffee = subset(market, market$COFFEE == 1)

no_coffee_tea = no_coffee$TEA
coffee_tea = yes_coffee$TEA

mean_no_coffee_tea = mean(no_coffee_tea)
mean_no_coffee_tea

mean_coffee_tea = mean(coffee_tea)
mean_coffee_tea

PermutationTestSecond::Permutation(market, "COFFEE", "TEA", 10000, "0", "1")
# p-value was 0.1298; failed to reject null