#!/usr/bin/python

import itertools
import re

def _matches(restriction, assignment):
  for key,value in restriction.items():
    if not (key in assignment and re.match(value, assignment[key])):
      return False
  return True

def _makeCmd(template, assignment):
  cmd = template
  row = {}

  for key,value in assignment.items():
    def replacer(m):
      row[key] = value
      # first handle possibility of <key>
      if re.match('<{}>'.format(key), m.group(0)):
        return value
      # now check for <key?foo>
      replacementMatch = re.match(r'<{}\?([^>]*)>'.format(key), m.group(0))
      if replacementMatch:
        return re.sub(r'({})', lambda _: value, replacementMatch.group(1))
      raise Exception("Unexpected match failure")
    pattern = r'(<{}>)|(<{}\?[^>]*>)'.format(key, key)
    cmd = re.sub(pattern, replacer, cmd)

  cmd = re.sub(r'(<[-\w]+\?[^>]*>)', lambda _: "", cmd)
  row['cmd'] = cmd.strip()

  return row

def gencmds(templateSpecs, assignmentGroupSpecs):
  rows = []
  seen = set()

  for aSpec in assignmentGroupSpecs:
    x = [[(k, v) for v in vs] for k,vs in aSpec]
    # x = [[(k, v) for v in vs] for k,vs in aSpec.items()]
    for assignment in itertools.product(*x):
      assignment = dict(assignment)
      for tSpec in templateSpecs:
        if _matches(tSpec['match'], assignment):
          row = _makeCmd(tSpec['template'], assignment)
          if not (row['cmd'] in seen or seen.add(row['cmd'])):
            rows.append(row)
          break

  return rows

# scripty part =============================================================

if __name__ == "__main__":
  import argparse, sys, os, json

  def json_careful_loads(s):
    try:
      return json.loads(s)
    except Exception as e:
      sys.stderr.write("[ERR] Error while parsing json: {}\n".format(e))
      sys.exit(1)

  thisDir = os.path.dirname(os.path.realpath(sys.argv[0]))
  defaultTemplatesFile = "{}/templates.json".format(thisDir)

  parser = argparse.ArgumentParser()

  parser.add_argument('-v', '--verbose', action='store_true', dest='verbose')

  parser.add_argument('-t', '--templates',
    default = defaultTemplatesFile,
    metavar = 'FILE',
    dest = 'templates_file',
    help = """parse command templates from FILE (json format); if omitted,
           defaults to 'SCRIPT_DIR/templates.json' (SCRIPT_DIR = directory
           containing gencmds.py)""")

  parser.add_argument('-b', '--bare',
    action = 'store_true',
    dest = 'bare',
    help = "don't print each generated command with its assigned keys (json format)")

  parser.add_argument('spec', metavar='SPEC_FILE',
    help = "key/value assignments (json format)")

  args = parser.parse_args()

  def eprint(msg):
    sys.stderr.write(msg)
    sys.stderr.write('\n')

  def vprint(msg):
    if args.verbose:
      eprint(msg)

  try:
    with open(args.templates_file, 'r') as f:
      message = "[INFO] Using templates file '{}'"
      vprint(message.format(args.templates_file))
      templateSpecs = json_careful_loads(f.read())
  except IOError:
    warning = ("[WARN] Cannot find templates file '{}'; " +
               'using default template "<prog> <args>" instead.')
    eprint(warning.format(args.templates_file))
    templateSpecs = [{'match': {}, 'template': '<prog> <args>'}]

  def hook(kvs):
    return kvs

  try:
    with open(args.spec, 'r') as f:
      vprint("[INFO] Reading command specs from '{}'".format(args.spec))
      assignmentGroupSpecs = json.loads(f.read(), object_pairs_hook=hook)
  except IOError:
    eprint("[ERR] Cannot find file '{}'\n".format(args.spec))
    sys.exit(1)

  for aSpec in assignmentGroupSpecs:
    for key, _ in aSpec:
      if not re.match(r'^[-\w]*$', key):
        msg = "Invalid key '{}'; must consist of only alphanumeric characters and '_' or '-'"
        eprint(msg.format(key))
        sys.exit(1)
      if key == 'cmd':
        msg = "Invalid key 'cmd'; this key is reserved"
        eprint(msg.format(key))
        sys.exit(1)

  for row in gencmds(templateSpecs, assignmentGroupSpecs):
    if args.bare:
      print(row['cmd'])
    else:
      print(json.dumps(row))

