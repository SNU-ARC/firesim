""" This file manages the overall configuration of the system for running
simulation tasks. """

from __future__ import print_function

import argparse
from datetime import timedelta
from time import strftime, gmtime
import pprint
import logging
import yaml

from fabric.api import * # type: ignore
from awstools.awstools import *
from awstools.afitools import *
from runtools.firesim_topology_with_passes import FireSimTopologyWithPasses
from runtools.workload import WorkloadConfig
from runtools.run_farm import RunFarm
from util.streamlogger import StreamLogger
import os
import sys
from absl import flags # type: ignore

LOCAL_DRIVERS_BASE = "../sim/output/f1/"
LOCAL_DRIVERS_GENERATED_SRC = "../sim/generated-src/f1/"
LOCAL_SYSROOT_LIB = "../sim/lib-install/lib/"
CUSTOM_RUNTIMECONFS_BASE = "../sim/custom-runtime-configs/"

rootLogger = logging.getLogger()

FLAGS = flags.FLAGS
flags.DEFINE_string(name='runtimeconfigfile', short_name='c', default='config_runtime.yaml', help='Optional custom runtime/workload config file.')
flags.DEFINE_string(name='hwdbconfigfile', short_name='a', default='config_hwdb.yaml', help='Optional custom HW database config file.')
flags.DEFINE_spaceseplist(name='overrideconfigdata', short_name='x', default=None, help='Override a single value from one of the the RUNTIME e.g.: --overrideconfigdata "target-config link-latency 6405".')

class RuntimeHWConfig:
    """ A pythonic version of the entires in config_hwdb.yaml """

    def __init__(self, name, hwconfig_dict):
        self.name = name
        self.agfi = hwconfig_dict['agfi']
        self.deploytriplet = hwconfig_dict['deploy-triplet-override']
        if self.deploytriplet is not None:
            rootLogger.warning("{} is overriding a deploy triplet in your config_hwdb.yaml file.  Make sure you understand why!".format(name))
        self.customruntimeconfig = hwconfig_dict['custom-runtime-config']
        # note whether we've built a copy of the simulation driver for this hwconf
        self.driver_built = False

    def get_deploytriplet_for_config(self):
        """ Get the deploytriplet for this configuration. This memoizes the request
        to the AWS AGFI API."""
        if self.deploytriplet is not None:
            return self.deploytriplet
        rootLogger.debug("Setting deploytriplet by querying the AGFI's description.")
        self.deploytriplet = get_firesim_tagval_for_agfi(self.agfi,
                                                         'firesim-deploytriplet')
    def get_design_name(self):
        """ Returns the name used to prefix MIDAS-emitted files. (The DESIGN make var) """
        my_deploytriplet = self.get_deploytriplet_for_config()
        my_design = my_deploytriplet.split("-")[0]
        return my_design

    def get_local_driver_binaryname(self):
        """ Get the name of the driver binary. """
        return self.get_design_name() + "-f1"

    def get_local_driver_path(self):
        """ return relative local path of the driver used to run this sim. """
        my_deploytriplet = self.get_deploytriplet_for_config()
        drivers_software_base = LOCAL_DRIVERS_BASE + "/" + my_deploytriplet + "/"
        fpga_driver_local = drivers_software_base + self.get_local_driver_binaryname()
        return fpga_driver_local

    def get_local_runtimeconf_binaryname(self):
        """ Get the name of the runtimeconf file. """
        return "runtime.conf" if self.customruntimeconfig is None else os.path.basename(self.customruntimeconfig)

    def get_local_runtime_conf_path(self):
        """ return relative local path of the runtime conf used to run this sim. """
        my_deploytriplet = self.get_deploytriplet_for_config()
        drivers_software_base = LOCAL_DRIVERS_BASE + "/" + my_deploytriplet + "/"
        my_runtimeconfig = self.customruntimeconfig
        if my_runtimeconfig is None:
            runtime_conf_local = drivers_software_base + "runtime.conf"
        else:
            runtime_conf_local = CUSTOM_RUNTIMECONFS_BASE + my_runtimeconfig
        return runtime_conf_local

    def get_boot_simulation_command(self, slotid, all_macs,
                                              all_rootfses, all_linklatencies,
                                              all_netbws, profile_interval,
                                              all_bootbinaries, trace_enable,
                                              trace_select, trace_start, trace_end,
                                              trace_output_format,
                                              autocounter_readrate, all_shmemportnames,
                                              enable_zerooutdram, disable_asserts,
                                              print_start, print_end,
                                              enable_print_cycle_prefix):
        """ return the command used to boot the simulation. this has to have
        some external params passed to it, because not everything is contained
        in a runtimehwconfig. TODO: maybe runtimehwconfig should be renamed to
        pre-built runtime config? It kinda contains a mix of pre-built and
        runtime parameters currently. """

        # TODO: supernode support
        tracefile = "+tracefile=TRACEFILE" if trace_enable else ""
        autocounterfile = "+autocounter-filename-base=AUTOCOUNTERFILE"

        # this monstrosity boots the simulator, inside screen, inside script
        # the sed is in there to get rid of newlines in runtime confs
        driver = self.get_local_driver_binaryname()
        runtimeconf = self.get_local_runtimeconf_binaryname()

        def array_to_plusargs(valuesarr, plusarg):
            args = []
            for index, arg in enumerate(valuesarr):
                if arg is not None:
                    args.append("""{}{}={}""".format(plusarg, index, arg))
            return " ".join(args) + " "

        def array_to_lognames(values, prefix):
            names = ["{}{}".format(prefix, i) if val is not None else None
                     for (i, val) in enumerate(values)]
            return array_to_plusargs(names, "+" + prefix)

        command_macs = array_to_plusargs(all_macs, "+macaddr")
        command_rootfses = array_to_plusargs(all_rootfses, "+blkdev")
        command_linklatencies = array_to_plusargs(all_linklatencies, "+linklatency")
        command_netbws = array_to_plusargs(all_netbws, "+netbw")
        command_shmemportnames = array_to_plusargs(all_shmemportnames, "+shmemportname")
        command_dromajo = "+drj_dtb=" + all_bootbinaries[0] + ".dtb" +  " +drj_bin=" + all_bootbinaries[0] + " +drj_rom=" + all_bootbinaries[0] + ".rom"

        command_niclogs = array_to_lognames(all_macs, "niclog")
        command_blkdev_logs = array_to_lognames(all_rootfses, "blkdev-log")

        command_bootbinaries = array_to_plusargs(all_bootbinaries, "+prog")
        zero_out_dram = "+zero-out-dram" if (enable_zerooutdram) else ""
        disable_asserts_arg = "+disable-asserts" if (disable_asserts) else ""
        print_cycle_prefix = "+print-no-cycle-prefix" if not enable_print_cycle_prefix else ""

        # TODO supernode support
        dwarf_file_name = "+dwarf-file-name=" + all_bootbinaries[0] + "-dwarf"

        # TODO: supernode support (tracefile, trace-select.. etc)
        basecommand = """screen -S fsim{slotid} -d -m bash -c "script -f -c 'stty intr ^] && sudo ./{driver} +permissive $(sed \':a;N;$!ba;s/\\n/ /g\' {runtimeconf}) +slotid={slotid} +profile-interval={profile_interval} {zero_out_dram} {disable_asserts} {command_macs} {command_rootfses} {command_niclogs} {command_blkdev_logs}  {tracefile} +trace-select={trace_select} +trace-start={trace_start} +trace-end={trace_end} +trace-output-format={trace_output_format} {dwarf_file_name} +autocounter-readrate={autocounter_readrate} {autocounterfile} {command_dromajo} {print_cycle_prefix} +print-start={print_start} +print-end={print_end} {command_linklatencies} {command_netbws}  {command_shmemportnames} +permissive-off {command_bootbinaries} && stty intr ^c' uartlog"; sleep 1""".format(
            slotid=slotid,
            driver=driver,
            runtimeconf=runtimeconf,
            command_macs=command_macs,
            command_rootfses=command_rootfses,
            command_niclogs=command_niclogs,
            command_blkdev_logs=command_blkdev_logs,
            command_linklatencies=command_linklatencies,
            command_netbws=command_netbws,
            profile_interval=profile_interval,
            zero_out_dram=zero_out_dram,
            disable_asserts=disable_asserts_arg,
            command_shmemportnames=command_shmemportnames,
            command_bootbinaries=command_bootbinaries,
            trace_select=trace_select,
            trace_start=trace_start,
            trace_end=trace_end,
            tracefile=tracefile,
            trace_output_format=trace_output_format,
            dwarf_file_name=dwarf_file_name,
            autocounterfile=autocounterfile,
            autocounter_readrate=autocounter_readrate,
            command_dromajo=command_dromajo,
            print_cycle_prefix=print_cycle_prefix,
            print_start=print_start,
            print_end=print_end)

        return basecommand



    def get_kill_simulation_command(self):
        driver = self.get_local_driver_binaryname()
        # Note that pkill only works for names <=15 characters
        return """sudo pkill -SIGKILL {driver}""".format(driver=driver[:15])


    def build_fpga_driver(self):
        """ Build FPGA driver for running simulation """
        if self.driver_built:
            # we already built the driver at some point
            return
        # TODO there is a duplicate of this in runtools
        triplet_pieces = self.get_deploytriplet_for_config().split("-")
        design = triplet_pieces[0]
        target_config = triplet_pieces[1]
        platform_config = triplet_pieces[2]
        rootLogger.info("Building FPGA software driver for " + str(self.get_deploytriplet_for_config()))
        with prefix('cd ../'), \
             prefix('export RISCV={}'.format(os.getenv('RISCV', ""))), \
             prefix('export PATH={}'.format(os.getenv('PATH', ""))), \
             prefix('export LD_LIBRARY_PATH={}'.format(os.getenv('LD_LIBRARY_PATH', ""))), \
             prefix('source ./sourceme-f1-manager.sh'), \
             prefix('cd sim/'), \
             StreamLogger('stdout'), \
             StreamLogger('stderr'):
            localcap = None
            with settings(warn_only=True):
                driverbuildcommand = """make DESIGN={} TARGET_CONFIG={} PLATFORM_CONFIG={} f1""".format(design, target_config, platform_config)
                localcap = local(driverbuildcommand, capture=True)
            rootLogger.debug("[localhost] " + str(localcap))
            rootLogger.debug("[localhost] " + str(localcap.stderr))
            if localcap.failed:
                rootLogger.info("FPGA software driver build failed. Exiting. See log for details.")
                rootLogger.info("""You can also re-run '{}' in the 'firesim/sim' directory to debug this error.""".format(driverbuildcommand))
                sys.exit(1)

        self.driver_built = True


    def __str__(self):
        return """RuntimeHWConfig: {}\nDeployTriplet: {}\nAGFI: {}\nCustomRuntimeConf: {}""".format(self.name, self.deploytriplet, self.agfi, str(self.customruntimeconfig))


class RuntimeHWDB:
    """ This class manages the hardware configurations that are available
    as endpoints on the simulation. """

    def __init__(self):

        agfidb_configfile = None
        with open(FLAGS.hwdbconfigfile, "r") as yaml_file:
            agfidb_configfile = yaml.safe_load(yaml_file)

        agfidb_dict = agfidb_configfile

        self.hwconf_dict = {s: RuntimeHWConfig(s, v) for s, v in agfidb_dict.items()}

    def get_runtimehwconfig_from_name(self, name):
        return self.hwconf_dict[name]

    def __str__(self):
        return pprint.pformat(vars(self))


class InnerRuntimeConfiguration:
    """ Pythonic version of config_runtime.yaml """

    def __init__(self):

        runtime_dict = None
        with open(FLAGS.runtimeconfigfile, "r") as yaml_file:
            runtime_dict = yaml.safe_load(yaml_file)

        # override parts of the runtime conf if specified
        configoverrideval = FLAGS.overrideconfigdata
        if configoverrideval is not None:
            if not configoverrideval:
                ## handle overriding part of the runtime conf
                overridesection = configoverrideval[0]
                overridefield = configoverrideval[1]
                overridevalue = configoverrideval[2]
                rootLogger.warning("Overriding part of the runtime config with: ")
                rootLogger.warning("""[{}]""".format(overridesection))
                rootLogger.warning(overridefield + "=" + overridevalue)
                runtime_dict[overridesection][overridefield] = overridevalue

        runfarmtagprefix = "" if 'FIRESIM_RUNFARM_PREFIX' not in os.environ else os.environ['FIRESIM_RUNFARM_PREFIX']
        if runfarmtagprefix != "":
            runfarmtagprefix += "-"

        self.runfarmtag = runfarmtagprefix + runtime_dict['run-farm']['run-farm-tag']

        aws_resource_names_dict = aws_resource_names()
        if aws_resource_names_dict['runfarmprefix'] is not None:
            # if specified, further prefix runfarmtag
            self.runfarmtag = aws_resource_names_dict['runfarmprefix'] + "-" + self.runfarmtag

        self.f1_16xlarges_requested = int(runtime_dict['run-farm']['f1_16xlarges']) if 'f1_16xlarges' in runtime_dict['run-farm'] else 0
        self.f1_4xlarges_requested = int(runtime_dict['run-farm']['f1_4xlarges']) if 'f1_4xlarges' in runtime_dict['run-farm'] else 0
        self.m4_16xlarges_requested = int(runtime_dict['run-farm']['m4_16xlarges']) if 'm4_16xlarges' in runtime_dict['run-farm'] else 0
        self.f1_2xlarges_requested = int(runtime_dict['run-farm']['f1_2xlarges']) if 'f1_2xlarges' in runtime_dict['run-farm'] else 0

        self.run_instance_market = runtime_dict['run-farm']['run-instance-market']
        self.spot_interruption_behavior = runtime_dict['run-farm']['spot-interruption-behavior']
        self.spot_max_price = runtime_dict['run-farm']['spot-max-price']

        self.topology = runtime_dict['target-config']['topology']
        self.no_net_num_nodes = int(runtime_dict['target-config']['no-net-num-nodes'])
        self.linklatency = int(runtime_dict['target-config']['link-latency'])
        self.switchinglatency = int(runtime_dict['target-config']['switching-latency'])
        self.netbandwidth = int(runtime_dict['target-config']['net-bandwidth'])
        self.profileinterval = int(runtime_dict['target-config']['profile-interval'])

        if 'launch-instances-timeout-minutes' in runtime_dict['run-farm']:
            self.launch_timeout = timedelta(minutes=int(runtime_dict['run-farm']['launch-instances-timeout-minutes']))
        else:
            self.launch_timeout = timedelta() # default to legacy behavior of not waiting

        self.always_expand = runtime_dict['run-farm'].get('always-expand-runfarm', "yes") == "yes"

        # Default values
        self.trace_enable = False
        self.trace_select = "0"
        self.trace_start = "0"
        self.trace_end = "-1"
        self.trace_output_format = "0"
        self.autocounter_readrate = 0
        self.zerooutdram = False
        self.disable_asserts = False
        self.print_start = "0"
        self.print_end = "-1"
        self.print_cycle_prefix = True

        if 'tracing' in runtime_dict:
            self.trace_enable = runtime_dict['tracing'].get('enable') == "yes"
            self.trace_select = runtime_dict['tracing'].get('selector', "0")
            self.trace_start = runtime_dict['tracing'].get('start', "0")
            self.trace_end = runtime_dict['tracing'].get('end', "-1")
            self.trace_output_format = runtime_dict['tracing'].get('output-format', "0")
        if 'autocounter' in runtime_dict:
            self.autocounter_readrate = int(runtime_dict['autocounter'].get('read-rate', "0"))
        self.defaulthwconfig = runtime_dict['target-config']['default-hw-config']
        if 'host-debug' in runtime_dict:
            self.zerooutdram = runtime_dict['host-debug'].get('zero-out-dram') == "yes"
            self.disable_asserts = runtime_dict['host-debug'].get('disable-synth-asserts') == "yes"
        if 'synth-print' in runtime_dict:
            self.print_start = runtime_dict['synth-print'].get("start", "0")
            self.print_end = runtime_dict['synth-print'].get("end", "-1")
            self.print_cycle_prefix = runtime_dict['synth-print'].get("cycle-prefix", "yes") == "yes"

        self.workload_name = runtime_dict['workload']['workload-name']
        # an extra tag to differentiate workloads with the same name in results names
        self.suffixtag = runtime_dict['workload']['suffix-tag'] if 'suffix-tag' in runtime_dict['workload'] else None
        self.terminateoncompletion = runtime_dict['workload']['terminate-on-completion'] == "yes"

    def __str__(self):
        return pprint.pformat(vars(self))

class RuntimeConfig:
    """ This class manages the overall configuration of the manager for running
    simulation tasks. """

    def __init__(self):
        """ This reads runtime configuration files, massages them into formats that
        the rest of the manager expects, and keeps track of other info. """
        self.launch_time = strftime("%Y-%m-%d--%H-%M-%S", gmtime())

        # construct pythonic db of hardware configurations available to us at
        # runtime.
        self.runtimehwdb = RuntimeHWDB()
        rootLogger.debug(self.runtimehwdb)

        self.innerconf = InnerRuntimeConfiguration()
        rootLogger.debug(self.innerconf)

        # construct a privateip -> instance obj mapping for later use
        #self.instancelookuptable = instance_privateip_lookup_table(
        #    f1_16_instances + f1_2_instances + m4_16_instances)

        # setup workload config obj, aka a list of workloads that can be assigned
        # to a server
        self.workload = WorkloadConfig(self.innerconf.workload_name, self.launch_time,
                                       self.innerconf.suffixtag)

        self.runfarm = RunFarm(self.innerconf.f1_16xlarges_requested,
                               self.innerconf.f1_4xlarges_requested,
                               self.innerconf.f1_2xlarges_requested,
                               self.innerconf.m4_16xlarges_requested,
                               self.innerconf.runfarmtag,
                               self.innerconf.run_instance_market,
                               self.innerconf.spot_interruption_behavior,
                               self.innerconf.spot_max_price,
                               self.innerconf.launch_timeout,
                               self.innerconf.always_expand)

        # start constructing the target configuration tree
        self.firesim_topology_with_passes = FireSimTopologyWithPasses(
            self.innerconf.topology, self.innerconf.no_net_num_nodes,
            self.runfarm, self.runtimehwdb, self.innerconf.defaulthwconfig,
            self.workload, self.innerconf.linklatency,
            self.innerconf.switchinglatency, self.innerconf.netbandwidth,
            self.innerconf.profileinterval, self.innerconf.trace_enable,
            self.innerconf.trace_select, self.innerconf.trace_start, self.innerconf.trace_end,
            self.innerconf.trace_output_format,
            self.innerconf.autocounter_readrate, self.innerconf.terminateoncompletion,
            self.innerconf.zerooutdram, self.innerconf.disable_asserts,
            self.innerconf.print_start, self.innerconf.print_end,
            self.innerconf.print_cycle_prefix)

    def launch_run_farm(self):
        """ directly called by top-level launchrunfarm command. """
        self.runfarm.launch_run_farm()

    def terminate_run_farm(self):
        """ directly called by top-level terminaterunfarm command. """
        self.runfarm.terminate_run_farm()

    def infrasetup(self):
        """ directly called by top-level infrasetup command. """
        # set this to True if you want to use mock boto3 instances for testing
        # the manager.
        use_mock_instances_for_testing = False
        self.firesim_topology_with_passes.infrasetup_passes(use_mock_instances_for_testing)

    def boot(self):
        """ directly called by top-level boot command. """
        use_mock_instances_for_testing = False
        self.firesim_topology_with_passes.boot_simulation_passes(use_mock_instances_for_testing)

    def kill(self):
        use_mock_instances_for_testing = False
        self.firesim_topology_with_passes.kill_simulation_passes(use_mock_instances_for_testing)

    def run_workload(self):
        use_mock_instances_for_testing = False
        self.firesim_topology_with_passes.run_workload_passes(use_mock_instances_for_testing)



