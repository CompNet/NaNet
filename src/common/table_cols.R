# This scripts contains all table-related constants.
# 
# Vincent Labatut
# 01/2019
###############################################################################




###############################################################################
# arcs
COL_ARC <- "Arc"								# arc title
COL_ARC_ID <- "ArcId"							# unique arc id (overall)
COL_ARCS <- "Arcs"								# number of arcs

# arc stats
COL_ARCS_BY_CHAR <- "AvgArcsByCharacter"		# average number of arcs by character




###############################################################################
# volumes
COL_VOLUME <- "Volume"							# unique volume code (overall)
COL_VOLUME_ID <- "VolumeId"						# unique volume id (overall)
COL_VOLUMES <- "Volumes"						# number of volumes

# volume stats
COL_VOLUMES_BY_CHAR <- "AvgVolumesByCharacter"	# average number of volumes by character
COL_VOLUMES_BY_ARC <- "AvgVolumesByArc"			# average number of volumes by arc




###############################################################################
# chapters
COL_CHAP <- "Chapter" 							# chapter number, relative to a (novel) volume
COL_CHAP_ID <- "ChapterId"						# unique chapter id (overall)
COL_CHAPS <- "Chapters"							# number of chapters in a volume/arc
# TODO implement stat computation for chapters, which were added afterwards (to handle ASOIAF)




###############################################################################
# pages
COL_PAGE <- "Page"								# page number, relative to a volume
COL_PAGE_ID <- "PageId"							# unique page id (overall)
COL_PAGE_END <- "EndPage"						# end page number, relative to a volume
COL_PAGE_END_ID <- "EndPageId"					# unique id of an end page (overall)
COL_PAGE_START <- "StartPage"					# start page number, relative to a volume
COL_PAGE_START_ID <- "StartPageId"				# unique id of an end page (overall)
COL_PAGES <- "Pages"							# number of pages

# page stats
COL_PAGES_BY_CHAR <- "AvgPagesByCharacter"		# average number of pages by character
COL_PAGES_BY_SCENE <- "AvgPagesByScene"			# average number of pages by scene
COL_PAGES_BY_VOLUME <- "AvgPagesByVolume"		# average number of pages by volume
COL_PAGES_BY_ARC <- "AvgPagesByArc"				# average number of pages by arc




###############################################################################
# panels
COL_PANEL <- "Panel"							# panel number, relative to a volume
COL_PANEL_ID <- "PanelId"						# unique panel id (overall)
COL_PANEL_END <- "EndPanel"						# number of an end panel, relative to a volume
COL_PANEL_END_ID <- "EndPanelId"				# unique id of an end panel (overall)
COL_PANEL_START <- "StartPanel"					# number of a start panel, relative to a volume
COL_PANEL_START_ID <- "StartPanelId"			# unique id of a start panel (overall)
COL_PANELS <- "Panels"							# number of panels

# panel stats
COL_PANELS_BY_CHAR <- "AvgPanelsByCharacter"	# average number of panels by character
COL_PANELS_BY_PAGE <- "AvgPanelsByPage"			# average number of panels by page
COL_PANELS_BY_SCENE <- "AvgPanelsByScene"		# average number of panels by scene
COL_PANELS_BY_VOLUME <- "AvgPanelsByVolume"		# average number of panels by volume
COL_PANELS_BY_ARC <- "AvgPanelsByArc"			# average number of panels by arc




###############################################################################
# scenes
#COL_SCENE <- "Scene"							# check: not sure this is needed
COL_SCENE_ID <- "SceneId"						# unique scene id (overall)
COL_SCENE_END_ID <- "EndSceneId"				# unique id of an end scene (overall)
COL_SCENE_START_ID <- "StartSceneId"			# unique id of a start scene (overall)
COL_SCENES <- "Scenes"							# number of scenes

# scene position
COL_MATCH_BOTH <- "MatchBoth"					# scene both starts and ends with a page
COL_MATCH_END <- "MatchEnd"						# scene starts with a page
COL_MATCH_START <- "MatchStart"					# scene ends with a page

# scene stats
COL_SCENES_BY_CHAR <- "AvgScenesByCharacter"	# average number of scenes by character
COL_SCENES_BY_PAGE <- "AvgScenesByPage"			# average number of scenes by page
COL_SCENES_BY_VOLUME <- "AvgScenesByVolume"		# average number of scenes by volume
COL_SCENES_BY_ARC <- "AvgScenesByArc"			# average number of scenes by arc




###############################################################################
# characters
COL_CHAR <- "Character"							# unique character name (overall)
COL_CHARS <- "Characters"						# number of characters
COL_NAME <- "Name"								# proper name or description of the character (=COL_CHAR)
COL_NAMED <- "Named"							# whether the character has a proper name
COL_FREQ <- "Frequency"							# number of scenes
COL_NAME_SHORT <- "ShortName"					# short version of the name, for important characters only
COL_FILTER <- "Filter"							# whether the character is important ("Keep") or not ("Discard")
COLS_ATT_IGNORE <- c(							# list of columns not representing character attributes
	COL_NAME, COL_NAME_SHORT, 
	COL_FREQ, 
	COL_ARCS, COL_VOLUMES, COL_PAGES, COL_PANELS, COL_SCENES
)

# char stats
COL_CHARS_BY_PAGE <- "AvgCharactersByPage"		# average number of characters by page
COL_CHARS_BY_PANEL <- "AvgCharactersByPanel"	# average number of characters by panel
COL_CHARS_BY_SCENE <- "AvgCharactersByScene"	# average number of characters by scene
COL_CHARS_BY_VOLUME <- "AvgCharactersByVolume"	# average number of characters by volume
COL_CHARS_BY_ARC <- "AvgCharactersByArc"		# average number of characters by arc




###############################################################################
# publication date
COL_MONTH <- "Month"							# number of the publication month
COL_YEAR <- "Year"								# publication year




###############################################################################
# interactions
COL_CHAR_FROM <- "FromChar"						# unique name of the origin character
COL_CHAR_TO <- "ToChar"							# unique name of the target character
COL_OCCURRENCES <- "Occurrences"				# number of character co-occurrences
COL_DURATION <- "Duration"						# total duration of the character co-occurrences (in panels) 




###############################################################################
# correlation
COL_NONE_SPEAR <- "spearman"					# Spearman's correlation computed for the unweighted graph
COL_NONE_PVAL <- "p-value"						# Corresponding p-value
COL_DUR_SPEAR <- "spearman-duration"			# Spearman's correlation computed for the durations
COL_DUR_PVAL <- "p-duration"					# Corresponding p-value
COL_OCC_SPEAR <- "spearman-occurrences"			# Spearman's correlation computed for the occurrences
COL_OCC_PVAL <- "p-occurrences"					# Corresponding p-value

#
COL_CORR_PANELS_CHARS_BY_SCENE <- "CorrelationPanelsCharactersByScene"
COL_CORR_SCENES_PANELS_BY_CHAR <- "CorrelationScenesPanelsByCharacter"




###############################################################################
# misc
COL_SERIES <- "Series"							# name of the series
COL_TITLE <- "Title"							# title of the volume
COL_RANK <- "Rank"								# order in the narrative story (by opposition to the publication order)
