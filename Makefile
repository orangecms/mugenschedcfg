COMPONENT = mugenschedcfg

include ../component.mk
include ../coverage.mk

include ../../projects/tests.mk

PLAN_XML   = plans/complex.xml
PLAN_RAW   = $(OBJ_DIR)/scheduling_plans
DEBUG_OUT  = $(OBJ_DIR)/debug.log
PDFVIEWER ?= mupdf

PLOTS_CONFIG = 1_-D 2_-D 4

plot: $(COMPONENT)
	rm -f $(OBJ_DIR)/scheduling_plans*
	$(MUGENSCHEDCFG) -r $(PLAN_RAW) -d $(DEBUG_OUT) $(PLAN_XML)
	cd $(OBJ_DIR) && gnuplot<../plot.gnuplot
	$(PDFVIEWER) $(OBJ_DIR)/scheduling_plans.pdf &

plots: $(COMPONENT)
	rm -f $(OBJ_DIR)/scheduling_plans*
	for i in $(PLOTS_CONFIG); do \
		cpu=`(echo $$i | cut -d_ -f1)`; \
		dis=`(echo $$i | cut -s -d_ -f2)`; \
		$(MUGENSCHEDCFG) -r $(PLAN_RAW) -d $(DEBUG_OUT).$$cpu -c $$cpu $$dis $(PLAN_XML) || break; \
		cd $(OBJ_DIR) && gnuplot<../plot.gnuplot; \
		$(PDFVIEWER) scheduling_plans.pdf & \
		cd ..; \
	done

fuzz: $(COMPONENT)
	for i in `seq 10`; do \
		r=`date +%N | sed 's/^0*//'`; \
		cpu=$$(($$r % 24)); \
		if [ $$(($$r % 2)) -eq 1 ]; then dis=-D; fi; \
		$(MUGENSCHEDCFG) -r $(PLAN_RAW) -d $(DEBUG_OUT).$$cpu -c $$cpu $$dis $(PLAN_XML) || break; \
		cd $(OBJ_DIR) && gnuplot<../plot.gnuplot; \
		$(PDFVIEWER) scheduling_plans.pdf & \
		cd ..; \
	done
