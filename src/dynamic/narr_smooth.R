# This script extracts a dynamic network using the narrative smoothing method
# proposed in
# 		X. Bost, V. Labatut, S. Gueye, and G. Linarès, 
#		“Narrative smoothing: dynamic conversational network for the analysis of TV Series plots,” 
#		2nd International Workshop on Dynamics in Networks (DyNo/ASONAM), 2016, pp. 1111–1118.
#		DOI: 10.1109/ASONAM.2016.7752379
# 
# Vincent Labatut
# 02/2022
###############################################################################





###############################################################################
# Use narrative smoothing to extract a dynamic network based on the specified
# scenes. This function is adapted from the Python version of Xavier Bost,
# available there:
# https://github.com/bostxavier/Narrative-Smoothing/blob/c306c5bd94240dfbc8f5ea918857c8acf392ed81/network_processing.py#L52
# 
# stats.chars: list of characters with their attributes.
# char.scenes: which character appears in which scene.
# stats.scenes: allows retrieving scene durations.
# 
# returns: a sequence of graphs corresponding to a dynamic graph.
###############################################################################
narrative.smoothing <- function(stats.chars, char.scenes, stats.scenes)
{	tlog(2, "Extracting a dynamic network using narrative smoothing")
	
	# loop over the characters for the first end point
	for(i in 1:(nrow(stats.char)-1))
	{	i.name <- stats.char[i,COL_STATS_CHAR]
		tlog(4, "Processing character #i=",i.name,"(",i,"/",(nrow(stats.char)-1),")")
		
		# loop over the remaining characters for the second end point
		for(j in (i+1):nrow(stats.char))
		{	j.name <- stats.char[j,COL_STATS_CHAR]
			tlog(6, "Processing character #j=",j.name,"(",j,"/",nrow(stats.char),")")
			
			# check whether the characters interact at least once
			idx <- which(sapply(char.scenes, function(chars) length(intersect(c(i.name, j.name),chars))==2))
			if(length(idx)>0)
			{	# compute relation weight before first occurrence
				
				before <- c()
				for(s in 1:idx[1])
				{	
					
				}
				
				# compute relation weight between two consecutive occurrences
				
				
				# compute relation weight after last occurrence
				
			}
		}
	}
	
	return(g)
}

before_time = []
for scene_idx in range(scene_indices[0]):
		
		# interaction time with other characters in current scene
		fSpk_sep_time = np.sum([e[3] for e in R.edges(fSpk, data='weight', keys=True) if e[2] == scene_idx])
sSpk_sep_time = np.sum([e[3] for e in R.edges(sSpk, data='weight', keys=True) if e[2] == scene_idx])
before_time.append(fSpk_sep_time + sSpk_sep_time)

before_time = np.array(before_time)

# either character has interacted with others before the first occurrence of the relationship
if np.any(before_time):
			# first scene in which (at least) one of the two characters interacted with others
			first_occ_idx = np.argmax(before_time > 0)

# narrative anticipation on the next occurrence of the relation
cum_from_next = np.flip(np.cumsum(np.flip(before_time)))
narr_anticip = edges[scene_indices[0]]['weight'] - cum_from_next

# add edges
for scene_idx in range(first_occ_idx, scene_indices[0]):
		S.add_edge(fSpk,
				sSpk,
				key=scene_idx,
				weight=_sigmoid(narr_anticip[scene_idx]),
				episode=scene_mapping[scene_idx])

###############################################################
# compute relation weight between two consecutive occurrences #
###############################################################

for k in range(len(scene_indices)):
		
		# index of last occurrence
		last_idx = scene_indices[k]

# update graph
# last occurrence
S.add_edge(fSpk,
		sSpk,
		key=last_idx,
		weight=_sigmoid(edges[last_idx]['weight']),
		episode=scene_mapping[last_idx])

if k < len(scene_indices) - 1:
			# index of next occurrence if any
			next_idx = scene_indices[k+1]

# interaction time with other characters in-between
sep_time = []

for scene_idx in range(last_idx+1, next_idx):
		
		# interaction time with other characters in current scene
		fSpk_sep_time = np.sum([e[3] for e in R.edges(fSpk, data='weight', keys=True) if e[2] == scene_idx])
sSpk_sep_time = np.sum([e[3] for e in R.edges(sSpk, data='weight', keys=True) if e[2] == scene_idx])
sep_time.append(fSpk_sep_time + sSpk_sep_time)

cum_from_last = np.cumsum(sep_time)
cum_from_next = np.flip(np.cumsum(np.flip(sep_time)))

# narrative persistence of the last occurrence of the relation
narr_persist = edges[last_idx]['weight'] - cum_from_last

# narrative anticipation on the next occurrence of the relation
narr_anticip = edges[next_idx]['weight'] - cum_from_next

# weights of the relationship between the last and next occurrences
weights = np.max(np.array([narr_persist, narr_anticip]), axis=0)

# update graph
# between last and next occurrences
for i in np.arange(weights.shape[0]):
		S.add_edge(fSpk,
				sSpk,
				key=last_idx+i+1,
				weight=_sigmoid(weights[i]),
				episode=scene_mapping[last_idx+i+1])

# next occurrence
S.add_edge(fSpk,
		sSpk,
		key=next_idx,
		weight=_sigmoid(edges[next_idx]['weight']),
		episode=scene_mapping[next_idx])

#################################################
# compute relation weight after last occurrence #
#################################################

after_time = []
for scene_idx in range(scene_indices[-1] + 1, len(scene_mapping)):
		
		# interaction time with other characters in current scene
		fSpk_sep_time = np.sum([e[3] for e in R.edges(fSpk, data='weight', keys=True) if e[2] == scene_idx])
sSpk_sep_time = np.sum([e[3] for e in R.edges(sSpk, data='weight', keys=True) if e[2] == scene_idx])
after_time.append(fSpk_sep_time + sSpk_sep_time)

after_time = np.array(after_time)

# either character has interacted with others after the last occurrence of the relationship
if np.any(after_time):
			
			# last scene in which (at least) one of the two characters interacted with others
			last_occ_idx = after_time.shape[0] - np.argmax(np.flip(after_time) > 0) - 1

# narrative persistence of the last occurrence of the relation
cum_from_last = np.cumsum(after_time)
narr_persist = edges[scene_indices[-1]]['weight'] - cum_from_last

# add edges
for k in range(last_occ_idx + 1):
		scene_idx = k + scene_indices[-1] + 1
S.add_edge(fSpk,
		sSpk,
		key=scene_idx,
		weight=_sigmoid(narr_persist[k]),
		episode=scene_mapping[scene_idx])

return S
