// Component for visualisation of found difference
// Author: Lukas Petr

import { useState } from 'react';
import { PropTypes } from 'prop-types';

import Container from 'react-bootstrap/Container';
import Col from 'react-bootstrap/Col';
import Row from 'react-bootstrap/Row';
import Alert from 'react-bootstrap/Alert';

import Callstack, { CallType } from './Callstack';
import Code from './Code';
import { DefinitionsPropTypes, DiffPropTypes } from '../PropTypesValues';

/**
 * @param {Object} props
 * @param {string} props.compare Name of compared function.
 * @param {Object} props.diff Part of yaml containing info about difference.
 * @param {string} props.oldFolder Name of old folder (only name of subdir).
 * @param {string} props.newFolder Name of new folder (only name of subdir).
 * @param {Object} props.definition Part of yaml containing definitions.
 * @param {Function} props.getFile Function for getting content of files.
 */
export default function Difference({
  compare,
  diff,
  oldFolder,
  newFolder,
  definitions,
  getFile,
}) {
  // Function which code to show (defaultly showing the differing function).
  const [functionToShow, setFunctionToShow] = useState({
    // If there is no call stack, the differing function is the compared function.
    name: diff['old-callstack'].length > 0
      ? diff['old-callstack'][diff['old-callstack'].length - 1].name
      : compare,
    // Assuming that the differing function is located in both call stacks.
    type: CallType.Both,
  });
  let codeBlock = null;
  let errorCodeMessage = null;
  // extracting only name (putting away kind)
  const name = functionToShow.name.split(' ')[0];
  const { type } = functionToShow;

  const needsOldCode = (type === CallType.Both || type === CallType.Old);
  const needsNewCode = (type === CallType.Both || type === CallType.New);
  const containsDefinition = (name in definitions);
  const containsOldDef = containsDefinition && 'old' in definitions[name];
  const containsNewDef = containsDefinition && 'new' in definitions[name];
  // If need old/new code then it has to contain old/new defintion.
  // Note: implication X => Y <=> not X or Y
  if ((!needsOldCode || containsOldDef) && (!needsNewCode || containsNewDef)) {
    const oldDefintion = definitions[name]?.old;
    const newDefintion = definitions[name]?.new;

    // Returns tuple (old, new) line where is called next function in the call stack.
    // The old/new can be set to -1 if there is no next called function (in case
    // the functionToShow is located only in one call stack).
    // If there is no called function for both functions (it is differing fun)
    // returns undefined.
    const getCallingLine = () => {
      // Note: If there will be cases of calls with different names and we will
      // want to show their code at the same time and the calls will be
      // calling other functions, then this will need to be reworked.
      let oldLine = -1;
      let newLine = -1;
      if (functionToShow.name === compare
          && diff['old-callstack'].length > 0 && diff['new-callstack'].length > 0) {
        oldLine = diff['old-callstack'][0].line;
        newLine = diff['new-callstack'][0].line;
      } else {
        const oldIndex = diff['old-callstack'].findIndex(
          (call) => call.name === functionToShow.name,
        );
        const newIndex = diff['new-callstack'].findIndex(
          (call) => call.name === functionToShow.name,
        );
        // Was the function found and it is not the differing function?
        if (oldIndex >= 0 && oldIndex < diff['old-callstack'].length - 1) {
          oldLine = diff['old-callstack'][oldIndex + 1].line;
        }
        if (newIndex >= 0 && newIndex < diff['new-callstack'].length - 1) {
          newLine = diff['new-callstack'][newIndex + 1].line;
        }
      }
      if (oldLine === -1 && newLine === -1) return undefined;
      return [oldLine, newLine];
    };
    if ((needsOldCode && !('end-line' in oldDefintion))
        || (needsNewCode && !('end-line' in newDefintion))) {
      errorCodeMessage = 'Missing info about ending of function.';
    } else {
      // creating component for code visualisation
      // if all information are included for showing code of function
      const specification = {
        oldSrc: needsOldCode ? oldDefintion.file : null,
        newSrc: needsNewCode ? newDefintion.file : null,
        diff: type === 'both' && definitions[name].diff
          ? `${name}.diff`
          : null,
        oldStart: needsOldCode ? oldDefintion.line : null,
        newStart: needsNewCode ? newDefintion.line : null,
        oldEnd: needsOldCode ? oldDefintion['end-line'] : null,
        newEnd: needsNewCode ? newDefintion['end-line'] : null,
        calling: getCallingLine(),
        type,
      };
      codeBlock = (
        <Code
          key={`${compare}-${functionToShow.name}`}
          specification={specification}
          getFile={getFile}
          oldFolder={oldFolder}
          newFolder={newFolder}
        />
      );
    }
  } /* if (name in definitions) */ else {
    errorCodeMessage = 'Missing info about definitions of functions.';
  }

  return (
    <Container fluid className="difference">
      <Row>
        <Col className="callstack-col" xs="12" xl="1" xxl="2">
          <Callstack
            compFunName={compare}
            oldCallStack={diff['old-callstack']}
            newCallStack={diff['new-callstack']}
            onSelect={setFunctionToShow}
            selectedFunction={functionToShow}
            definitions={definitions}
          />
        </Col>
        <Col>
          {errorCodeMessage ? (
            <Alert>
              Unable to show code:
              {' '}
              {errorCodeMessage}
            </Alert>
          ) : (
            codeBlock
          )}
        </Col>
      </Row>
    </Container>
  );
}

Difference.propTypes = {
  compare: PropTypes.string.isRequired,
  diff: DiffPropTypes.isRequired,
  oldFolder: PropTypes.string.isRequired,
  newFolder: PropTypes.string.isRequired,
  definitions: DefinitionsPropTypes.isRequired,
  getFile: PropTypes.func.isRequired,
};
