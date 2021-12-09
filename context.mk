DEPS += lib-basic

PROGRAM := libutils

$(PROGRAM).configure: SOURCE := tagion/**/*.d

# Since we only cross-compile for mobile, we can switch off
# Term.d when cross compilation mode is enabled
ifdef CROSS_ENABLED
DCFLAGS+=$(DVERSION)=MOBILE
endif