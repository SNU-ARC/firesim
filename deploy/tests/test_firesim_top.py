import logging
import pytest
from pytest_mock import MockerFixture
from textwrap import dedent
import re
import sure

import firesim
from firesim import register_task, FiresimTaskAccessViolation
from runtools.runtime_config import RuntimeConfig

rootLogger = logging.getLogger()

# In case you put any package-level tests, make sure they use the test credentials too
pytestmark = pytest.mark.usefixtures("aws_test_credentials")

def test_task_with_no_parameters(mocker: MockerFixture):
    mocker.patch.dict(firesim.TASKS, clear=True)

    def my_favorite_task():
        pass

    firesim.TASKS.should_not.contain('my_favorite_task')
    register_task(my_favorite_task)
    firesim.TASKS.should.contain('my_favorite_task')

def test_task_with_annotated_parameter(mocker: MockerFixture):
    mocker.patch.dict(firesim.TASKS, clear=True)

    def my_favorite_task(config: RuntimeConfig):
        pass

    firesim.TASKS.should_not.contain('my_favorite_task')
    register_task(my_favorite_task)
    firesim.TASKS.should.contain('my_favorite_task')

class FirstReg:
    def duplicate_task(self, config: RuntimeConfig):
        pass

class SecondReg:
    def duplicate_task(self, config: RuntimeConfig):
        pass

def test_duplicate_registration(mocker: MockerFixture):
    mocker.patch.dict(firesim.TASKS, clear=True)

    # it is more likely that these functions would be in two different modules maybe but
    # for the purposes of testing, having them be in two different classes works too

    one = FirstReg()
    two = SecondReg()

    firesim.TASKS.should.be.empty
    register_task(one.duplicate_task)
    firesim.TASKS.should.contain('duplicate_task')
    register_task.when.called_with(two.duplicate_task).should.throw(KeyError, re.compile(r'Task.*already registered by'))

@pytest.mark.parametrize('tn', list(firesim.TASKS.keys()) + list(firesim.TASKS.keys()))
def test_main_dispatching(mocker: MockerFixture, task_mocker, tn: str):
    mocker.patch.dict('os.environ', {'FIRESIM_SOURCED': '1'})
    parser = firesim.construct_firesim_argparser()

    task_mocker.patch(tn)

    args = parser.parse_args([tn])
    firesim.main(args)
    if firesim.TASKS[tn]['config']:
        firesim.TASKS[tn]['task'].assert_called_once()
        firesim.TASKS[tn]['config'].assert_called_once_with(args)
    else:
        firesim.TASKS[tn]['task'].assert_called_once_with()

def test_decorated_task_callability():
    firesim.managerinit.when.called_with().should.throw(FiresimTaskAccessViolation)
