// copy-runtime-deps.mjs
import fs from 'node:fs';
import path from 'node:path';

const [src, dest, ...roots] = process.argv.slice(2);
const seen = new Set();

function find(name, from) {
  // Node-style resolution: nearest node_modules walking up, bounded by src
  let dir = from;
  while (true) {
    const candidate = path.join(dir, 'node_modules', name);
    if (fs.existsSync(path.join(candidate, 'package.json'))) return candidate;
    if (path.resolve(dir) === path.resolve(path.dirname(src))) return null;
    const parent = path.dirname(dir);
    if (parent === dir) return null;
    dir = parent;
  }
}

function visit(name, from) {
  const pkgDir = find(name, from);
  if (!pkgDir) return; // optional deps may be absent
  const real = fs.realpathSync(pkgDir);
  if (seen.has(real)) return;
  seen.add(real);
  const pkg = JSON.parse(fs.readFileSync(path.join(real, 'package.json'), 'utf8'));
  const target = path.join(dest, path.relative(path.resolve(src), real));
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.cpSync(real, target, { recursive: true });
  for (const dep of Object.keys({ ...pkg.dependencies, ...pkg.optionalDependencies })) {
    visit(dep, real);
  }
}

for (const r of roots) visit(r, path.dirname(path.resolve(src)));
