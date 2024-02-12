#//////////////////////////////////////////////////////////////////////////////////////
#//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
#//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
#//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
#//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
#//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
#//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
#//                                                                                  //
#//  Originally developed by D. Pizzocri & T. Barani                                 //
#//                                                                                  //
#//  Version: 2.0                                                                    //
#//  Year: 2022                                                                      //
#//  Authors: T. Barani.                                                             //
#//                                                                                  //
#//////////////////////////////////////////////////////////////////////////////////////

CC          := g++	# Windows and Linux
# CC := /opt/homebrew/bin/g++-12	# Mac OS

TARGET      := sciantix.x

SRCDIR      := src
INCDIR      := include
BUILDDIR    := obj
TARGETDIR   := bin
RESDIR      := src
SRCEXT      := C
DEPEXT      := d
OBJEXT      := o

CFLAGS      := -Wall -O -g
LIB         := -lm
INC         := -I$(INCDIR) -I/usr/local/include #also this may change according to your environment specs
INCDEP      := -I$(INCDIR)

SOURCES     := $(shell find $(SRCDIR) -type f -name *.$(SRCEXT))
OBJECTS     := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.$(SRCEXT)=.$(OBJEXT)))

#####################################################################################

all: resources $(TARGET)

remake: cleaner all

resources: directories
	@cp $(RESDIR)/* $(TARGETDIR)/

directories:
	@mkdir -p $(TARGETDIR)
	@mkdir -p $(BUILDDIR)

clean:
	@$(RM) -rf $(BUILDDIR)
	@$(RM) -rf $(TARGETDIR)/*.$(SRCEXT)

cleaner: clean
	@$(RM) -rf $(TARGETDIR)

-include $(OBJECTS:.$(OBJEXT)=.$(DEPEXT))

$(TARGET): $(OBJECTS)
	$(CC) -o $(TARGETDIR)/$(TARGET) $^ $(LIB)

$(BUILDDIR)/%.$(OBJEXT): $(SRCDIR)/%.$(SRCEXT)
	@mkdir -p $(dir $@)
	@$(CC) $(CFLAGS) $(INC) -c -o $@ $<
	@echo "Compiling C++ "$<" ... "
	@$(CC) $(CFLAGS) $(INCDEP) -MM $(SRCDIR)/$*.$(SRCEXT) > $(BUILDDIR)/$*.$(DEPEXT)
	@cp -f $(BUILDDIR)/$*.$(DEPEXT) $(BUILDDIR)/$*.$(DEPEXT).tmp
	@sed -e 's|.*:|$(BUILDDIR)/$*.$(OBJEXT):|' < $(BUILDDIR)/$*.$(DEPEXT).tmp > $(BUILDDIR)/$*.$(DEPEXT)
	@sed -e 's/.*://' -e 's/\\$$//' < $(BUILDDIR)/$*.$(DEPEXT).tmp | fmt -1 | sed -e 's/^ *//' -e 's/$$/:/' >> $(BUILDDIR)/$*.$(DEPEXT)
	@rm -f $(BUILDDIR)/$*.$(DEPEXT).tmp
	@$(RM) -rf $(TARGETDIR)/*.$(SRCEXT)

.PHONY: all remake clean cleaner resources
