proc main()
	local a[10], b
	for i =  1 to 9 
		a[i] = i
	end_for

	for i = 0 to 9
		b = a[i]
		say b
	end_for
end_proc