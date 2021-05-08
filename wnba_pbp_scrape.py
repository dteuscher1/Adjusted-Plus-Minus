import pandas as pd
test = pd.read_html("https://www.espn.com/wnba/playbyplay/_/gameId/401104913")
test_data = pd.concat([test[1], test[2], test[3], test[4]], ignore_index=True)
test_data
