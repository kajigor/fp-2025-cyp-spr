import subprocess
import csv
import matplotlib.pyplot as plt

# Запускаем и читаем вывод построчно (реальное время)
process = subprocess.Popen(
    ["stack", "run", "metrics-one-run"],
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    text=True
)

lines = []
print("\n📡 Realtime Output:\n" + "-" * 40)
for line in process.stdout:
    print(line.strip())
    lines.append(line)

process.wait()

header = lines[0].strip()
rows = [list(map(float, line.strip().split(","))) for line in lines[1:] if "," in line]

plt.figure(figsize=(10, 5))
plt.subplot(1, 2, 1)
sizes = sorted(set(r[0] for r in rows))
for size in sizes:
    dps = [r[1] for r in rows if r[0] == size]
    ratios = [r[4] for r in rows if r[0] == size]
    plt.plot(dps, ratios, marker="o", label=f"Size {int(size)}")
plt.title("Ratio vs Depth (by Size)")
plt.xlabel("Depth")
plt.ylabel("charCalls / length")
plt.legend()
plt.grid(True)

plt.subplot(1, 2, 2)
depths = sorted(set(r[1] for r in rows))
for depth in depths:
    szs = [r[0] for r in rows if r[1] == depth]
    ratios = [r[4] for r in rows if r[1] == depth]
    plt.plot(szs, ratios, marker="o", label=f"Depth {int(depth)}")
plt.title("Ratio vs Size (by Depth)")
plt.xlabel("Size")
plt.ylabel("charCalls / length")
plt.legend()
plt.grid(True)

plt.tight_layout()
plt.show()
