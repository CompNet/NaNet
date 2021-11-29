# This scripts contains all table-related constants.
# 
# Vincent Labatut
# 01/2019
###############################################################################


###############################################################################
# column names for the volume table
COL_VOLS_ARC <- "Arc"
COL_VOLS_LENGTH <- "Length"
COL_VOLS_MONTH <- "Month"
COL_VOLS_PAGE_END <- "End"
COL_VOLS_PAGE_START <- "Start"
COL_VOLS_SERIES <- "Series"
COL_VOLS_TITLE <- "Title"
COL_VOLS_VOLUME <- "Volume"
COL_VOLS_VOLUME_ID <- "VolumeId"
COL_VOLS_YEAR <- "Year"
COL_VOLS_RANK <- "Rank"


###############################################################################
# column names for the page table
COL_PAGES_VOLUME <- "Volume"
COL_PAGES_PAGE <- "Page"
COL_PAGES_PAGE_ID <- "PageId"
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
COL_CHAR_NAME <- "Name"				# proper name or description of the character
COL_CHAR_FREQ <- "Frequency"		# number of scenes
COL_CHAR_SHORT_NAME <- "ShortName"	# short version of the name, for important characters only
COL_CHAR_NAMED <- "Named"			# whether the character has a proper name
#COL_CHAR_SEX <- "Sex"				# sex of the character
COL_CHAR_FILTERED <- "Filtered"		# important character (FALSE) or extra (TRUE)


###############################################################################
# column names for the correlation table
COL_SPEAR_DUR <- "spearman-duration"
COL_PVAL_DUR <- "p-duration"
COL_SPEAR_OCC <- "spearman-occurrences"
COL_PVAL_OCC <- "p-occurrences"


###############################################################################
# column names for the stats tables
COL_STATS_ARC <- "Arc"
COL_STATS_ARC_ID <- "ArcId"
COL_STATS_CHAR <- "Character"
COL_STATS_CHARS <- "Characters"
COL_STATS_CHARS_BY_PAGE <- "AvgCharactersByPage"
COL_STATS_CHARS_BY_PANEL <- "AvgCharactersByPanel"
COL_STATS_CHARS_BY_SCENE <- "AvgCharactersByScene"
COL_STATS_CHARS_BY_VOLUME <- "AvgCharactersByVolume"
COL_STATS_CORR_PANELS_CHARS_BY_SCENE <- "CorrelationPanelsCharactersByScene"
COL_STATS_CORR_SCENES_PANELS_BY_CHAR <- "CorrelationScenesPanelsByCharacter"
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
COL_STATS_PAGES_BY_CHAR <- "AvgPagesByCharacter"
COL_STATS_PAGES_BY_SCENE <- "AvgPagesByScene"
COL_STATS_PAGES_BY_VOLUME <- "AvgPagesByVolume"
COL_STATS_PANEL <- "Panel"
COL_STATS_PANEL_ID <- "PanelId"
COL_STATS_PANELS <- "Panels"
COL_STATS_PANELS_BY_CHAR <- "AvgPanelsByCharacter"
COL_STATS_PANELS_BY_PAGE <- "AvgPanelsByPage"
COL_STATS_PANELS_BY_SCENE <- "AvgPanelsByScene"
COL_STATS_PANELS_BY_VOLUME <- "AvgPanelsByVolume"
COL_STATS_SCENE <- "Scene"
COL_STATS_SCENES <- "Scenes"
COL_STATS_SCENES_BY_CHAR <- "AvgScenesByCharacter"
COL_STATS_SCENES_BY_PAGE <- "AvgScenesByPage"
COL_STATS_SCENES_BY_VOLUME <- "AvgScenesByVolume"
COL_STATS_START_PAGE <- "StartPage"
COL_STATS_START_PAGE_ID <- "StartPageId"
COL_STATS_START_PANEL <- "StartPanel"
COL_STATS_START_PANEL_ID <- "StartPanelId"
COL_STATS_VOLUME <- "Volume"
COL_STATS_VOLUMES <- "Volumes"
COL_STATS_VOLUMES_BY_CHAR <- "AvgVolumesByCharacter"
COL_STATS_VOLUME_ID <- "VolumeId"
