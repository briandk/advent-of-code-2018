input = []

##############

with open('Untitled.txt', 'r') as f:
	input = tuple(
		f.read()
			.replace('\n', ',')[:-1] # strip the last comma
			.split(',')
	)
	
	
# count distunct letter frequencies

def count_letters(word):
	letter_counts = {}
	for character in word:
		if character in letter_counts:
			letter_counts[character] += 1
		else:
			letter_counts[character] = 1
	return letter_counts
	

def has_exactly_n_characters(word, n):
	has_n_characters = False
	counts = count_letters(word)
	letters_with_count_n = [
		{k, v} for (k, v) in counts.items()
			if (v is n)
	]
	
	if (len(letters_with_count_n) > 0):
		has_n_characters = True

	return has_n_characters
	
def compute_checksum(words):
	words_with_two_letters = 0
	words_with_three_letters = 0
	for word in words:
		if (has_exactly_n_characters(word, 2)):
			words_with_two_letters += 1
		if (has_exactly_n_characters(word, 3)):
			words_with_three_letters += 1
	print('two count: ', words_with_two_letters)	
	print('three count: ', words_with_three_letters)
	return words_with_three_letters * words_with_two_letters

	
print(has_exactly_n_characters('ccc', 3))	
print(compute_checksum(['aa', 'bb', 'ccc']))

print('checksum is: ', compute_checksum(input))
			
#############

def compare_two_words(w1, w2):
	common_characters = list()
	discrepancy = 0
	for (position, character) in enumerate(w1):
		if (character is w2[position]):
			common_characters.append(w1[position])
		else:
			discrepancy += 1
	return (discrepancy, "".join(common_characters))
	
print(compare_two_words('food', 'fool')[1])

def find_matching_ids(ids):
	mstches = list()
	for word1 in ids:
		for word2 in ids:
			comparison = compare_two_words(word1, word2)
			if (comparison[5] > 10):
				print(comparison)
				
				
print([
	compare_two_words(w1, w2)
		for w1 in input
			for w2 in input
][:10])
