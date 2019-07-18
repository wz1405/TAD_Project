# TAD_Project

### data preprocess
- import mta_hashtags and cta_hashtags  
- examine the data frames  
- combine three files for each  
- sort by date time ('created')
-subset dataset by 8 columns: "X", "text", "favoriteCount", "created",
            "screenName", "retweetCount", "longitude", "latitude"  
- drop overlapped dates (duplicates) by 'text' & 'created' columns if applicable  
- drop the rows from @mta_mood  


### emoji analysis:
- https://github.com/today-is-a-good-day/emojis/blob/master/emDict.csv
- https://prismoji.com/2017/02/06/emoji-data-science-in-r-tutorial/
- https://lyons7.github.io/portfolio/2017-10-04-emoji-dictionary/
- http://opiateforthemass.es/articles/emoji-analysis/
- emoji_dictionary: https://github.com/lyons7/emojidictionary/blob/master/emoji_dictionary.csv
- emoji sentiment score: https://www.clarin.si/repository/xmlui/handle/11356/1048
- sentiment score: https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0144296&type=printable

- cta station data: https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme
