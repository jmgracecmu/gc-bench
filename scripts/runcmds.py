#!/usr/bin/python

import sys, signal, socket, time, threading, subprocess
from datetime import datetime

_currentChild = None
def _signalHandler(signal, frame):
  sys.stderr.write("[ERR] Interrupted.\n")
  if _currentChild:
    _currentChild.kill()
  sys.exit(1)
signal.signal(signal.SIGINT, _signalHandler)

def _killer():
  if _currentChild:
    try:
      _currentChild.kill()
    except Exception as e:
      sys.stderr.write("[WARN] Error while trying to kill process {}: {}\n".format(_currentChild.pid, str(e)))

def runcmds(rows, repeat=1, timeout=300.0, silent=False):
  repeat = max(repeat, 1)
  numCmds = len(rows) * repeat
  cmdNum = 1

  for row in rows:
    for _ in xrange(0, repeat):
      row = row.copy()
      if not silent:
        sys.stderr.write('[{}/{}] {}\n'.format(cmdNum, numCmds, row['cmd']))
      cmdNum += 1
      row['host'] = socket.gethostname()
      row['timestamp'] = datetime.now().strftime("%y-%m-%d %H:%M:%S.%f")
      if 'cwd' in row:
        _currentChild = subprocess.Popen(row['cmd'], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=row['cwd'])
      else:
        _currentChild = subprocess.Popen(row['cmd'], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
      timer = threading.Timer(timeout, _killer)
      row['stdout'] = ''
      row['stderr'] = ''
      ts = time.time()
      try:
        timer.start()
        row['stdout'], row['stderr'] = _currentChild.communicate()
      finally:
        timer.cancel()
        row['elapsed'] = time.time() - ts
      row['returncode'] = _currentChild.returncode
      yield row

# scripty part =============================================================

if __name__ == "__main__":
  import argparse, json, os.path

  parser = argparse.ArgumentParser()
  parser.add_argument('-r', '--repeat', type=int, default=1, dest='repeat')
  parser.add_argument('-t', '--timeout', type=float, default=300.0, dest='timeout')
  parser.add_argument('-s', '--silent', action='store_true', dest='silent')
  parser.add_argument('-o', '--output', type=argparse.FileType('a'), default=sys.stdout, dest='output')
  parser.add_argument('-b', '--bare', action = 'store_true', dest = 'bare')
  args = parser.parse_args()

  if args.bare:
    rows = [{'cmd':x.rstrip('\n')} for x in sys.stdin]
  else:
    rows = [ json.loads(x) for x in sys.stdin ]

  binsToMake = []
  for r in rows:
    binFile = '{}.{}.bin'.format(r['bench'], r['config'])
    if not os.path.isfile('bin/' + binFile) and (binFile not in binsToMake):
      binsToMake.append(binFile)

  if len(binsToMake) > 0:
    sys.stderr.write("[WARN] missing binaries:\n")
    for b in binsToMake:
      sys.stderr.write("  " + b + "\n")

    cmd = "make -j " + (" ".join(binsToMake))
    sys.stderr.write("[INFO] " + cmd + "\n")
    out = subprocess.check_output(cmd, shell=True, stderr=subprocess.STDOUT)
    sys.stderr.write(out + "\n")

  for result in runcmds(rows, repeat=args.repeat, timeout=args.timeout, silent=args.silent):
    s = '{}\n'.format(json.dumps(result))
    args.output.write(s)
    if not args.silent:
      sys.stderr.write(result['stdout'] + '\n')
      sys.stderr.write(result['stderr'] + '\n')
