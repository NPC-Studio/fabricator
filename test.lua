local sum = 0;
for i = 0,10000000 do
  sum = sum + i
end

print(os.clock())
print(sum)
