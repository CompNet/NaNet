# This scripts contains all table-related constants.
# 
# Vincent Labatut
# 01/2019
###############################################################################


###############################################################################
# column names for the volume table
COL_VOLS_VOLUME <- "Volume"
COL_VOLS_START_PAGE <- "Start"
COL_VOLS_END_PAGE <- "End"
COL_VOLS_LENGTH <- "Length"
COL_VOLS_TITLE <- "Title"


###############################################################################
# column names for the page table
COL_PAGES_VOLUME <- "Volume"
COL_PAGES_PAGE <- "Page"
COL_PAGES_PANELS <- "Panels"
COL_PAGES_START_PANEL_ID <- "StartPanelId"
COL_PAGES_VOLUME_ID <- "VolumeId"


###############################################################################
# column names for the interaction table
#COL_INTER_VOLUME <- "Volume"
#COL_INTER_START_PAGE <- "StartPage"
#COL_INTER_START_PANEL <- "StartPanel"
#COL_INTER_END_PAGE <- "EndPage"
#COL_INTER_END_PANEL <- "EndPanel"
#COL_INTER_END_CHAR <- "Characters"
COL_INTER_FROM_CHAR <- "FromChar"
COL_INTER_TO_CHAR <- "ToChar"
COL_INTER_START_PANEL_ID <- "StartPanelId"
COL_INTER_END_PANEL_ID <- "EndPanelId"
COL_INTER_OCCURRENCES <- "Occurrences"
COL_INTER_DURATION <- "Duration"


###############################################################################
# column names for the character table
COL_CHAR_NAME <- "Name"


###############################################################################
# column names for the correlation table
COL_SPEAR_DUR <- "spearman-duration"
COL_PVAL_DUR <- "p-duration"
COL_SPEAR_OCC <- "spearman-occurrences"
COL_PVAL_OCC <- "p-occurrences"


###############################################################################
# column names for the stats tables
COL_STATS_CHAR <- "Character"
COL_STATS_CHAR_BY_PAGE <- "AvgCharacterByPage"
COL_STATS_CHAR_BY_SCENE <- "AvgCharacterByScene"
COL_STATS_CHARS <- "Characters"
COL_STATS_END_PAGE <- "EndPage"
COL_STATS_END_PAGE_ID <- "EndPageId"
COL_STATS_END_PANEL <- "EndPanel"
COL_STATS_END_PANEL_ID <- "EndPanelId"
COL_STATS_MATCH_BOTH <- "MatchBoth"
COL_STATS_MATCH_END <- "MatchEnd"
COL_STATS_MATCH_START <- "MatchStart"
COL_STATS_PAGE <- "Page"
COL_STATS_PAGE_ID <- "PageId"
COL_STATS_PAGES <- "Pages"
COL_STATS_PANEL <- "Panel"
COL_STATS_PANEL_BY_PAGE <- "AvgPanelByPage"
COL_STATS_PANEL_BY_SCENE <- "AvgPanelByScene"
COL_STATS_PANEL_ID <- "PanelId"
COL_STATS_PANELS <- "Panels"
COL_STATS_SCENE <- "Scene"
COL_STATS_SCENES <- "Scenes"
COL_STATS_START_PAGE <- "StartPage"
COL_STATS_START_PAGE_ID <- "StartPageId"
COL_STATS_START_PANEL <- "StartPanel"
COL_STATS_START_PANEL_ID <- "StartPanelId"
COL_STATS_VOLUME <- "Volume"
COL_STATS_VOLUMES <- "Volumes"
COL_STATS_VOLUME_ID <- "VolumeId"

#- distribution/avg value of 
#	x panels/page
#	x panel/scene
#	x characters/scene
#	x scene/characters
#		- number of scenes for the main characters
#	- char/scene vs. scene duration (heatmap, cf. LREC paper)
#- avg/counts by volume
#- proportion of scenes 
#	- starting on a page fist panel
#	- ending on a page last panel
#	- both at the same time
