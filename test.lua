local sum = 0;
for i = 0,1000000 do
  sum = sum + i
end

print(os.clock())
print(sum)
