#this creates a python script that 
#parses an .asc file into fixations
#to replace eyedoctor
# Written by Adrian Staub, with mods by Chuck Clifton
# to permit use of proportional or fixed fonts
# as well as multi-line input files.
# Title RobodocPS.py
# March 22 2015

#NOTE:  This is a version that can handle multi-line input files.  

#makes .da1 file for each subject
#each line represents one trial, looks like this:
#1 2 12 9049 7 0 0 11 7 0 5159 5298 3 0 5315 5612 
#sequence condition itemnumber totaltrialtime exitcode 0 0 
#numberfixations xcoord ycoord starttime endtime ....

# leaving out the 0 0 for now
# it's for display change, which probably has to be dealt with later

#note that this script does not output any trial that ends with 
#a 'repeated' message.
#this is good.

#import modules
import os, sys
import math

parameters = input("What is the name of your parameter file?")

try:
	parameters = open(parameters,'r')
	#this reads the file as text
	whole_file = parameters.read()
	#this executes the whole thing as code
	exec(whole_file)
	
except:
	print ("Your parameter file could not be found.  Or, there is an error in the file.")
	sys.exit(0)
	
if blink_reg_exclude == "y":
	try:
		region_file = open(region_file, 'r')
	except:
		print ("Your region file could not be found.")
		sys.exit(0)

		
#and make a subject summary file
sumsub = open('sum.sub', 'w')
	
for filename in file_list:
	print(filename)
	counter = 0			# count for sequence in experiment
	try:
		input_asc = open(filename, 'r')
	except:
		print("File %s could not be found." %filename)
		sys.exit(0)

# set up to keep record of actual position in sequence
	trial_sequence = []
		
	#strip off extension
	output = filename[:-4]
	
	#Set da1 directory
	da1_dir = os.getcwd() + '/DA1'
	if not os.path.exists(da1_dir): os.makedirs(da1_dir)
	
	#add .da1 extension to filename
	output_da1 =  da1_dir + "/" + output + ".da1"
	output_da1 = open(output_da1, 'w')
	
	#Set da1 directory
	bli_dir = os.getcwd() + '/bli'
	if not os.path.exists(bli_dir): os.makedirs(bli_dir)

	#do the same things to make a blink file
	output_bli = bli_dir + "/" + output + ".da1"
	output_bli = open(output_bli, 'w')
	
	#define var to hold trial exclusions
	num_exclusions = 0


	#make a file that contains only the trialid lines and efix and blink lines
	#also need display on line and trial_result line
	copy_it = 0
	search_strings = ['TRIALID', 'DISPLAY ON', 'EFIX','EBLINK', 'TRIAL_RESULT','REGION CHAR']
	tempfile = open('tempfile','w+')
	for line in input_asc:
		for entry in search_strings:
			if entry in line:
				fields = line.split()#break line into list
				if search_strings[0] in fields:
					counter = counter + 1
					#now getting the trialid info
					trialid = fields[3]
					first_split = trialid.split('I')#split into the condition, and then item and dependent
					condition = first_split[0]
					cond_num = condition[1:] #strip off the letter from the beginning of condition
					second_split = first_split[1].split('D')#split into item and dependent
					item_num = second_split[0]
					dep_num = second_split[1]
					if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and int(dep_num) >= 0 and trialid[0] == 'E':
						copy_it = 1
						trial_sequence = trial_sequence + [counter]
				if copy_it:
					tempfile.write(line)
				if search_strings[4] in fields:
					copy_it = 0							
	tempfile.close()
		
	#now go through tempfile

	#make a string to hold all the entries for a row in the output file
	output_string_trial = []

	#make a counter for the trial sequence
	trial_seq = 0


    # fields is the array of values on a given line of a trial
	tempfile = open('tempfile', 'r')
	for line in tempfile:
		row = line
		fields = row.split()#break line into list
	
		#this is for the trialid lines
		if search_strings[0] in fields:
	
			#a bunch of variables that need to be initialized 
			#when a trialid line is en`countered
			trial_exclude = 0
			x_coord_char = 0
			x_coord_prev = 0
			trial_start_time = 0
			output_string_fix = []
			output_string_blink = []
			blink_end_times = []
			numfix = 0
			current_dur = 0
			previous_dur = 0
			repeat = 0
			crossed_into_crit = 0
			crossed_outof_crit = 0
			blink_check_start = 0
			blink_check_end = 0
        # stuff for character positions
			temp_xpos = 0
			temp_xpos_R = 0
			temp_ypos = 0
			XPOS = []
			XPOS_R = []			# right edge of char
			YPOS = []
		
			#now getting the trialid info
			trialid = fields[3]
			trialid_time = int(fields[1])
			first_split = trialid.split('I')#split into the condition, and then item and dependent
			condition = first_split[0]
			cond_num = condition[1:] #strip off the letter from the beginning of condition
			second_split = first_split[1].split('D')#split into item and dependent
			item_num = second_split[0]
			output_string_trial = [str(trial_sequence[trial_seq]),str(cond_num), str(item_num)]
		
			#get character position of critical region for blink exclusion
			if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
				for line in region_file:
					row2 = line
					fields2 = row2.split()#break line into list
					if fields2[0] == str(item_num) and fields2[1] == str(cond_num):
						blink_region_char_start = int(fields2[blink_region + 2])
						blink_region_char_end = int(fields2[blink_region + 3])
						blink_y_start = 0
						blink_y_end = 0
						if multi_line == "y":
							while blink_region_char_start >= 160:
								blink_region_char_start = blink_region_char_start - 160
								blink_y_start = blink_y_start+1
							while blink_region_char_end >= 160:
								blink_region_char_end = blink_region_char_end - 160
								blink_y_end = blink_y_end+1
#				print("   COND item x y start x y end",cond_num,item_num,blink_region_char_start, blink_y_start,blink_region_char_end,blink_y_end)
#have to put region file pointer back at top
				region_file.seek(0,0)
			# let's get the character positions - they come right after TRIALID, labeled REGION CHAR
		elif 'REGION' in fields:
			temp_xpos = int(fields[-4])
			temp_xpos_R = int(fields[-2])
			if multi_line == "n":
				temp_ypos = 0
			else:
				top_left_y = int(first_line_y - (line_sep_y/2))
				temp_ypos = int((int(fields[-3]) - top_left_y)/line_sep_y)
#				print("Y Pos",temp_ypos,int(fields[-3]))
			XPOS = XPOS + [temp_xpos]
			XPOS_R = XPOS_R + [temp_xpos_R]
			YPOS = YPOS + [temp_ypos]
            

		#this is for display on line
		elif 'DISPLAY' in fields:
			trial_start_time = int(fields[1])
#			print(trial_start_time)
		
		#this is for the EFIX lines
		#need to ignore the fixation if the trial start time has not been set yet
		elif search_strings[2] in fields:
			if trial_start_time == 0:
				#this doesn't do anything
				foo = fields[1]
			else:
				#this updates the value of previous_dur
				#to be the duration from the previous trial
				previous_dur = current_dur
				#and this updates the value of 
				#x_coord_prev to be the value of x_coord_char from
				#the previous trial
				x_coord_prev = x_coord_char
			
				start_time_raw = fields[2]
				end_time_raw = fields[3]
				#adjust to start time of trial
				start_time_adj = int(start_time_raw) - int(trial_start_time)
				if start_time_adj < 0:
					start_time_adj = 0
				end_time_adj = int(end_time_raw) - int(trial_start_time)
				current_dur = end_time_adj - start_time_adj
				y_coord_pix = fields[6]
				#this is to put in zeros when you are using single line tracking, otherwise
				#adjust to reflect which line you are on
				if(multi_line == "n"):
					y_coord_char = 0
				else:
					y_coord_char = int((int(y_coord_pix) - first_line_y)/line_sep_y)
#					print("y_coord_pix",y_coord_pix,"first_line_y",first_line_y,"line_sep_y",line_sep_y,"y_coord_char",y_coord_char)
# now get X position
				x_coord_pix = float(fields[5])
				xi = 0
				yi = 0
# test for outside range Y fixations
				if((y_coord_char > int(YPOS[-1])) | (y_coord_char < 0)):
#					yi = -1						# outside range
					y_coord_char = -1
					xi = -1
#					print("outside range",output_string_trial,"y",y_coord_char)
				else:
					while(y_coord_char > YPOS[yi]):
						yi = yi + 1
#					print("Set yi for y_char",yi,y_coord_char)
# test for outside range X fixations on this line
					yyi = yi
					while((yyi < len(YPOS)-1) & (YPOS[yyi] == YPOS[yi])):
#						print("Cond Item", cond_num,item_num,"x_coord y_coord",x_coord_pix,y_coord_pix,"len_YPOS", len(YPOS),"yyi yi",yyi, yi, "XPOS[yyi] YPOS[yyi] YPOS[yi]", XPOS[yyi],YPOS[yyi],YPOS[yi])
#						print("Cond Item x_coord y_coord len_YPOS yyi yi XPOS[yyi] YPOS[yyi] YPOS[yi]",cond_num,item_num,x_coord_pix,y_coord_pix,len(YPOS),yyi, yi, XPOS[yyi],YPOS[yyi],YPOS[yi])
						yyi = yyi+1
# find last XPOS in line
#					print("end loop: yyi, XPOS[yyi]",yyi,XPOS[yyi])
#					if((x_coord_pix < float(XPOS[0])) | (x_coord_pix > float(XPOS[yyi-1]))):
# let eye fix in the last character
					if((x_coord_pix < float(XPOS[0])) | (x_coord_pix > float(XPOS_R[yyi]))):
#					if((x_coord_pix < float(XPOS[0])) | (x_coord_pix > float(XPOS[yyi]))):
						xi = -1						# outside range
#						print("outside range",output_string_trial,"x",x_coord_pix,"x_coord_pix",x_coord_pix,"XPOS[yyi-1]",XPOS[yyi-1],"yyi",yyi)
					elif(yi >= 0):
						while(x_coord_pix > float(XPOS_R[yi])):
#						while(x_coord_pix > float(XPOS[yi])):
							xi = xi + 1
							yi = yi + 1                
# yi counts position in array; xi position in array relative to current line
#					print("xi yi",xi,yi)
				x_coord_char = xi
				numfix += 1
				temp_eye_data = [str(x_coord_char), str(y_coord_char), str(start_time_adj), str(end_time_adj)]
				output_string_fix = output_string_fix + temp_eye_data
			
			
				#this checks if the fixation you just added is within one
				#character of previous fixation
				#doesn't do it if this is the first fix
				if abs(x_coord_char - x_coord_prev) <= 1 and numfix > 1 and x_coord_char != -1 and y_coord_char != -1:
					#check if current duration is less than criterion
					#and remove last fixation if so
					#and add time to previous fix
					if current_dur < short_crit:
#						print(output_string_fix)
						del output_string_fix[-4:]
						#print(output_string_fix)
						newdur = int(output_string_fix[-1]) + current_dur
						output_string_fix[-1] = str(newdur)
						numfix = numfix - 1
						#print(output_string_fix)
					elif previous_dur < short_crit: #this deletes the previous fixation
					#and adds time to the current one
						#print(output_string_fix)
						del output_string_fix[-8:-4]
						#print(output_string_fix)
						newdur = int(output_string_fix[-2]) - previous_dur
						output_string_fix[-2] = str(newdur)
						#print(output_string_fix)
						numfix = numfix - 1
					
				#this checks if you've crossed into the critical region yet
				#if you have, gets the end time of the penultimate fixation
				#and updates value of crossing in variable
				#nothing here needs to be changed for multi-line tracking
				if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
#					print("cond item x-fix,x_start,y-fix,y_start",cond_num,item_num,output_string_fix[-4],blink_region_char_start,output_string_fix[-3],blink_y_start)
					if int(output_string_fix[-4]) > blink_region_char_start and ((multi_line == "n") | (int(output_string_fix[-3]) == blink_y_start)):
#						print("got one")
						if crossed_into_crit == 0:
							if numfix <= 1:
								blink_check_start = 0
								crossed_into_crit = 1
							else:
								blink_check_start = int(output_string_fix[-5])
								crossed_into_crit = 1
								#this checks if you've violated the saccade dur limit
								#between this fixation and the previous one
								#and if so, chucks the trial
								if int(output_string_fix[-2]) - int(output_string_fix[-5]) > saccade_dur_crit:
									trial_exclude = 1
#								print("item,blink_check_start, fix-2, fix-5, exclude",item_num,blink_check_start,int(output_string_fix[-2]),int(output_string_fix[-5]),trial_exclude)
							
				#this checks if you've crossed out of the region
				#if you have, gets the start time of the current fix
				#and updates value of crossing out variable
				if blink_reg_exclude == 'y' and int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond:
					if int(output_string_fix[-4]) > blink_region_char_end and int(output_string_fix[-3]) == blink_y_end:
						if crossed_outof_crit == 0:
							if numfix <= 1:
								blink_check_end = int(output_string_fix[-2])
								crossed_outof_crit = 1
							else:
								blink_check_end = int(output_string_fix[-2])
								#and checks if you've violated the saccade dur limit
								#and chucks the trial, if so
								if int(output_string_fix[-2]) - int(output_string_fix[-5]) > saccade_dur_crit:
									trial_exclude = 1
								crossed_outof_crit = 1
#								print("item, blink_check_end, fix-2, fix-5, exclude",item_num,blink_check_end,int(output_string_fix[-2]),int(output_string_fix[-5]),trial_exclude)
							
		#this is for blink lines:
		elif search_strings[3] in fields:
			if trial_start_time == 0:
				#this doesn't do anything, in case blink is before actual start of trial
				foo = fields[1]
			else:
				start_time_raw = fields[2]
				end_time_raw = fields[3]
				#adjust to start time of trial
				start_time_adj = int(start_time_raw) - int(trial_start_time)
				end_time_adj = int(end_time_raw) - int(trial_start_time)
			
				#this checks if blink duration results in exclusion of trial
				if end_time_adj - start_time_adj >= blink_dur_crit:
					trial_exclude = 1

				#this saves the blink end time in a string
				#for later checking against critical times
				blink_end_times.append(str(end_time_adj))
				
				temp_blink_data = [str(start_time_adj), str(end_time_adj)]			
				output_string_blink = output_string_blink + temp_blink_data
			
				#this checks if the number of blinks is too many
				if len(output_string_blink)/2 > blink_num_crit:
					trial_exclude = 1
				

			
		#this is for trial_result lines
		elif search_strings[4] in fields:
	
			#update the trial sequence, since we're at the end of a trial
			trial_seq += 1
		
			trial_dur = int(fields[1]) - trial_start_time
			exit_code = fields[3]
			middle_part = [str(trial_dur), str(exit_code), str(numfix)]
			output_string_da1 = output_string_trial + middle_part + output_string_fix
			output_string_bli = output_string_trial + middle_part + output_string_blink
			
			#this checks if any blink was in the critical time window
			for time in blink_end_times:
#				print("cond,item,time,blink_check_start,blink_check_end",cond_num,item_num,time,blink_check_start,blink_check_end)
				if int(time) > blink_check_start and int(time) < blink_check_end:
					trial_exclude = 1
		
			#this is to output the line to the .da1 file and blink file
			#if the condition is included, and the trial is not excluded
			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and trial_exclude == 0:
				joined_da1 = ' '.join(output_string_da1)
				output_da1.write(joined_da1)
				output_da1.write("\n")
			
				#this is to output the line to the blink file
				joined_blink = ' '.join(output_string_bli)
				output_bli.write(joined_blink)
				output_bli.write("\n")
				
			if int(cond_num) >= lowest_cond and int(cond_num) <= highest_cond and trial_exclude == 1:
				num_exclusions += 1
				output_bli.write("reject: ")
				joined_blink = ' '.join(output_string_bli)
				output_bli.write(joined_blink)
				output_bli.write("\n")
				

	tempfile.close()		

	#this is to write out some stuff for each subject to a summary file
	sumsub.write('%r %d \n' %(filename, num_exclusions))
	
	output_da1.close()
	output_bli.close()
	

sumsub.close()