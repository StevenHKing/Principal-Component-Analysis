
# Today we assess market mapping via dimension reduction with 
# principal components analysis (PCA).
 
#### first I need to install the ggrepel, ggfortify, and corrplot packages

install.packages("ggrepel")
install.packages("ggfortify")
install.packages("corrplot")


library(tidyverse)
library(ggrepel)   # <-- helps label points on our plots
library(ggfortify) # <-- auto-plots PCA results
library(corrplot)  # <-- makes correlations prettier



cust_dat <- read_csv("/Users/cherylking/Desktop/MGT_100/Week 1 Data/smartphone_customer_data.csv")

    
# Let's map phones in 1-D attribute space based on their screen sizes 

    # create a small dataset with one row per phone
    sub <- cust_dat |> 
            select(years_ago, brand, screen_size, size_cat, phone_id) |> 
            arrange(years_ago, brand, screen_size, size_cat, phone_id) |> 
            distinct()
    sub
    
    # plot phones by size, one facet per year
    ggplot(sub, aes(x=screen_size, y=0)) +        # usual stuff, set y=0 to plot points on a horiz line
        facet_grid(rows=vars(years_ago)) +        # use facet_grid to create separate plots by years_ago
        geom_point(size=2) +                      # make paints a bit bigger with size=2
        geom_hline(yintercept=0) +                # add a horizontal line at y=0
        geom_text_repel(aes(label=phone_id)) +    # from ggrepel package, adds texts to points
        theme_bw()                                # change theme
    
    # in this 1D space, we see:
        # that Samsung's large phone are quite a bit bigger than Apple's and Huawei's in last 2 years
        # that phone sizes are increasing each year
        # that the range of phone sizes are increasing each year
        # that the ordering of phones is stable year to year
    
    # We'll come back to this chart as point of comparison later
    
    
    
    #### I think this chart would bring up an excellent discussion of Hotelling
    #### that was brought up in class. It would be interesting to see if the larger companies
    #### are attempting to move towards the average when the population prefers the extreme ranges more.
    
    

# Let's calculate avg handsize per phone.
# We'll treat this as an unobserved product attribute
    
    sub <- cust_dat |> 
            group_by(years_ago, brand, screen_size, size_cat, phone_id) |> 
            summarize(mhs = mean(handsize)) |> 
            arrange(years_ago, brand, screen_size, size_cat, phone_id)
    
    
# What would we expect to see in a screensize x handsize map?
    
    # Are larger phones bought by people with larger hands?
    
    #### Yes, and it's likely a difference in gender/sex demographics. Men have larger hands
    #### and on avergage prefer larger phones. 
    
    # Should handsize be increasing year to year?
    
    #### Not nesssesarily, as I mentioned above in the Hotelling comment, much of the population will
    #### prefer the extreme ranges of phone sizes.
    
    
    # Will phones of the same screen size by bought by people with the same handsize?
    
    #### That is what is most likely. The first iPhone was actually designed around 
    #### the hand size of Steve Jobs.
    
    
    # Will people with the same handsize by phones of different screen sizes?
    
    #### Yes, you will always see some overlap in the distribution. 
    
    # Should the handsize-to-phonesize relationship change over time?
    
    #### Yes, but it's likely that they should not all be increasing at the same rate.
    #### Companies may want to change sizes to further target different horizontal groups. 
    
    # Let's find out!
    
    # Plot consumers' screensize vs handsize, facet by phone, for only years_ago==1
    ggplot(cust_dat |> filter(years_ago==1)) + 
        geom_histogram(aes(handsize), bins=50) + 
        facet_grid(rows=vars(screen_size)) + 
        ggtitle("Hand Size Distributions by Screen Size") + 
        theme_bw()
    
    #### This verifies that people with larger hands do tend to prefer lager screens. 
    
    # Plot/map phones in screensize x handsize space
    ggplot(sub, aes(x=screen_size, y=mhs)) +   # 
        geom_point() +                         #
        facet_grid(rows=vars(years_ago)) +     # different plots for different years_ago
        geom_smooth(method="lm", se=F) +       # add linear trend (ie regression) line
        geom_text_repel(aes(label=phone_id)) + #
        theme_bw()                             # 
    
    # What do we see?
    
    #### As we move from 3 → 2 → 1 years ago, two things are happening at once:
    #### Average screen size increases (the whole market shifts rightward).
    #### The range widens (the distance between smallest and largest models grows). What we discussed in class...
    #### They're not only getting bigger overall, but they're also differentiating more, meaning brands are offering 
    #### both very small and very large phones to capture different user segments.
        
    # There is a positive relationship between screen size and hand size
    
    #### Yes, absolutely, people with larger hands do tend to prefer larger phones.
    
        # The relationship isn't perfect.  Look at facet 3:
            # S1 and H2 have similar screen sizes, but H2 tends to be bought by people with larger hands
            # S1 and A2 have different screen sizes, but tend to be bought by people with the same hand size
        # The relationship is softening just a little (tend line getting flatter) over time
    
    ##### # Additional observations:
    ##### Over time, phones appear not only to be getting larger on average,
    ##### but also spreading farther apart in size — suggesting increased market differentiation.
    ##### This likely reflects manufacturers targeting different consumer segments 
    ##### (e.g., smaller phones for smaller-handed or female users, larger phones for male users).
    ##### So even as the overall trend flattens, the range of options is widening.
    
    
    
# Let's use PCA to reduce our 2D (screensize, avg handsize) to 1D         #### OK :)
    # that is, we'll do a 1D map using both variables
    # To do this, we need a dimension reduction technique. 
    # Principal Components Analysis (PCA) is a useful, popular technique.
    # In R, we can do PCA with prcomp()
    
    pca_out1 <- sub |> ungroup() |> filter(years_ago == 1) |> select(screen_size, mhs) |> prcomp()
    pca_out2 <- sub |> ungroup() |> filter(years_ago == 2) |> select(screen_size, mhs) |> prcomp()
    pca_out3 <- sub |> ungroup() |> filter(years_ago == 3) |> select(screen_size, mhs) |> prcomp()
    
    summary(pca_out1)
    summary(pca_out2)
    summary(pca_out3)
    
    # We see that one component explains the majority of variance in the data
    
    #### Yes, PC1 explains over 98%-99% of the total variance in all three years.
    ##### This indicates that the two variables (screen size and mean hand size) are highly correlated.
    
    # This is because mean-handsize and screen size are highly correlated
    
    sub |> group_by(years_ago) |> summarize(cor=cor(mhs, screen_size))
    
    # Let's also compare the variance along the principal components to the 
    # variance alone the original dimensions
    
    summary(pca_out1)
    sub |> ungroup() |> select(screen_size, mhs) |> summarize_all(sd)
    
    # we see that the original data varied along both variables, while the rotated
    # data varies mostly along the first principal component
    
    ####### Steven King Comment: Can we just pause for a minute and think about how cool that is?
    ####### I've never taken linear algebra, but learning about PCA makes me want to. This is such a 
    ##### a cool concept. I could see how you could use this in all kins of things, like risk factors in finance. 
    
    
    # let's see what else is in pca_out
    
    str(pca_out1)
    
    # the rotated data is in pca$x
    # we can use the first column of x for our 1D plot
    
    # first, let's "extract" x into a tibble, to make it easier to work with
    pcs1 <- as_tibble(pca_out1$x)
    pcs2 <- as_tibble(pca_out2$x)
    pcs3 <- as_tibble(pca_out3$x)
    
    # and append these tibbles together
    pcs <- bind_rows(pcs1, pcs2, pcs3, .id="years_ago")
    
    # now we can plot in 1-D space
    ggplot(pcs, aes(x=PC1, y=0)) + 
        facet_grid(rows=vars(years_ago)) + 
        geom_point(size=3) + 
        geom_hline(yintercept=0) + 
        geom_text_repel(aes(label=sub$phone_id)) + 
        theme_bw()

    ###################### So Cool! This is brilliant. ######################
    
    
    # some observations
        # H1 is much further from the other phones now
        # A1 and S1 are now: further apart in year 1, closer together in year 2, and unchanged in year 3
        

# what if we look at the relationships between phone use among customer in our data
    
    # run PCA on the 6 usage variables
    cust_dat |> select(gaming:reading) |> prcomp() -> out2
    summary(out2)
    
    # plot points in 2-D component space with component vectors
    autoplot(out2, 
             color="dodgerblue4", alpha=0.2,                 # <-- change color & transparency of points
             loadings = TRUE, loadings.colour = "firebrick", # <-- add component vectors and select color
             loadings.label = TRUE, loadings.label.size = 5) # <-- label component vectors and increase font size
    
    # let's look at the correlations in the data to confirm relationships among phone-use variables
    cust_dat |> 
        select(gaming:reading) |> 
        cor() |> 
        corrplot::corrplot(method="number", type="lower")
    
    ##### This is wild...I feel like I'm seeing inside people's behavior, almost creepy, but cool. 
    ##### I am a little surprised that gaming and social are not more positively correlated.
    
    ######!*!*!*!*!*!*! NOT END OF LAB -- QUIZ 4 DATA AND QUESTIONS BELOW !*!*!*!*!*!*!
    
    
# Summary of R commands introduced

    # principal components analysis (PCA)
        # prcomp()

    # plotting text that doesn't overlap
        # ggrepel::geom_text_repel()
    
    # plotting the components and points on the same map
        # autoplot(out, loadings = TRUE)
    
    # calculating correlation matrices
        # cor()
        # corrplot::corplot()
    
    # optional: new tidyverse commands
        # ungroup()    # to override default tibble behavior created from group_by()
        # distinct()   # to get unique combinations
        # geom_hline   # to plot a horizontal line
        # as_tibble()  # to convert a matrix to a tibble
        # bind_rows()  # to "stack" tibbles
        
