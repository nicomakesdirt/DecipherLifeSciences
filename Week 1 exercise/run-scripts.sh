# Exercise 1: Unix
# Author: Nico Hawley-Weld
# Date: Jan 24, 2024

# INSTRUCTIONS
# Keep all the unix commands needed to perform tasks 7-16 here.
# Comment your file appropriately. 
# We will see this file when you submit your assignment.

# 6-7. See code-1.R

# 8. Move into the ~/project-1/code directory and and create a file called code-2.R in the code directory using the command touch.
cd ~/project-1/code
touch code-2.R

# 9. Use the following command to add a line to the file: echo "load('../rdas/murders.rda')" > code-2.R 
Use less to check if the line of code was added (do not add the less part to script).
echo "load('../rdas/murders.rda')" > code-2.R 

# 10. Change the name of code-2.R to analysis.R
mv code-2.R analysis.R

# 11. Navigate to the code directory and list all the files ending in .R.
ls *.R

# 12. Navigate back to the project-1 directory. Without navigating away, change the name of code-1.R to import.R, but keep the file in the same directory.
cd ..
mv code/code-1.R code/import.R

# 13. Change the name of the project-1 directory to murders. Describe what you have to change so the R script sill does the right thing and how this would be different if you had used full paths.
cd ..
mv project-1 murders
# Note: both R scripts should still work fine. If I had used full paths in the R script the "project-1" component of the path would need to change to "murders"

# 14. Navigate to the murders directory.  Use find to list all the files ending in .R in the murders directory or any of it's subdirectories. Note: we have not discussed find so you will have to read the man page.
cd murders
find . -name "*.R"

# 15. Navigate to the murders/code directory. Type Rscript import.R and use ls with relative paths to see if the dat.rda file was created in ~/murders/rdas/.
cd code
Rscript import.R
ls ../rdas

# Change the name of dat.rda to murders.rda . Then navigate to the murders/code directory. Type Rscript analysis.R It shoud run without errors.
cd ..
mv rdas/dat.rda rdas/murders.rda
cd code
Rscript analysis.R
