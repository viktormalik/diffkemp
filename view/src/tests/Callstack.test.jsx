// Unit tests of Callstack component
// Author: Lukas Petr

import {
  fireEvent, render, screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';

import Callstack from '../components/Callstack';
import { MacroFunctionDifference } from './examples';

describe('simple callstack visualisation test', () => {
  const compFunName = '__put_task_struct';
  const oldCallStack = [{ name: 'free_task' }, { name: 'free_task_struct' }];
  const newCallStack = [{ name: 'free_task' }, { name: 'free_task_struct' }];
  const onSelect = jest.fn();

  const setup = () => {
    render(
      <Callstack
        compFunName={compFunName}
        oldCallStack={oldCallStack}
        newCallStack={newCallStack}
        definitions={{}}
        onSelect={onSelect}
      />,
    );
  };
  test('all function names should be visible', () => {
    setup();
    expect(screen.getByText('__put_task_struct')).toBeVisible();
    expect(screen.getByText('free_task')).toBeVisible();
    expect(screen.getByText('free_task_struct')).toBeVisible();
  });
  test('click on function should call onSelect callback', () => {
    setup();
    fireEvent.click(screen.getByText('__put_task_struct'));
    expect(onSelect).toHaveBeenCalled();
  });
});

describe('complex callstack visualisation test', () => {
  // 193/240
  const compFunName = '_cond_resched';
  const oldCallStack = [
    { name: 'preempt_schedule_common' },
    { name: '__schedule' },
    { name: 'smp_processor_id (macro)' },
    { name: 'raw_smp_processor_id (macro)' },
    { name: 'this_cpu_read (macro)' },
    { name: '__pcpu_size_call_return (macro)' },
    { name: 'this_cpu_read_8 (macro)' },
    { name: 'percpu_from_op (macro)' },
  ];
  const newCallStack = [
    { name: 'preempt_schedule_common' },
    { name: '__schedule' },
    { name: 'smp_processor_id (macro)' },
    { name: '__smp_processor_id (macro)' },
    { name: '__this_cpu_read (macro)' },
    { name: 'raw_cpu_read (macro)' },
    { name: '__pcpu_size_call_return (macro)' },
    { name: 'raw_cpu_read_8 (macro)' },
    { name: 'percpu_from_op (macro)' },
  ];

  test('all function names should be visible', () => {
    render(
      <Callstack
        compFunName={compFunName}
        oldCallStack={oldCallStack}
        newCallStack={newCallStack}
        definitions={{}}
        onSelect={() => {}}
      />,
    );
    expect(screen.getByText(compFunName)).toBeVisible();
    oldCallStack.forEach(({ name }) => {
      expect(screen.getByTitle(name)).toBeVisible();
    });
    newCallStack.forEach(({ name }) => {
      expect(screen.getByTitle(name)).toBeVisible();
    });
  });
});

describe("callstack with 'different' differing functions", () => {
  test('all function names should be visible', () => {
    render(
      <Callstack
        compFunName={MacroFunctionDifference.results[0].function}
        oldCallStack={MacroFunctionDifference.results[0].diffs[0]['old-callstack']}
        newCallStack={MacroFunctionDifference.results[0].diffs[0]['new-callstack']}
        definitions={{}}
        onSelect={() => {}}
      />,
    );
    expect(screen.getByText('down_write')).toBeVisible();
    expect(screen.getByTitle('get_task_struct (macro)')).toBeVisible();
    expect(screen.getByTitle('get_task_struct')).toBeVisible();
  });
});

describe('callstack with renamed function', () => {
  const compFunName = 'comp_fun1';
  const oldCallStack = [{ name: 'old_name' }];
  const newCallStack = [{ name: 'new_name' }];
  const definitions = {
    old_name: {
      old: {},
      new: { name: 'new_name' },
    },
  };
  test('renamed function names should be visible', () => {
    render(
      <Callstack
        compFunName={compFunName}
        oldCallStack={oldCallStack}
        newCallStack={newCallStack}
        definitions={definitions}
        onSelect={() => {}}
      />,
    );
    expect(screen.getByText('old_name')).toBeVisible();
    expect(screen.getByText('new_name')).toBeVisible();
  });
});
