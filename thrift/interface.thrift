namespace * cds

typedef binary MasterKeyPart;
typedef list<MasterKeyPart> MasterKeyParts;
typedef binary Token;
typedef binary CardData;

enum GetCardDataError {
	UNKNOWN = 0
	LOCKED = 1
	NOT_FOUND = 2
}

enum PutCardDataError {
	UNKNOWN = 0
	LOCKED = 1
}

enum InitError {
	UNKNOWN = 0
	#KEYRING_EXISTS = 1
	BAD_ARGUMENTS = 3
}

enum UnlockError {
	UNKNOWN = 0
	NO_KEYRING = 1
	CANNOT_DECRYPT = 3
	CANNOT_RECOVER = 4
}

enum RotateError {
	UNKNOWN = 0
	LOCKED = 1
	BACKEND_LOCKED = 2
}

#enum RekeyError {
#	UNKNOWN = 0
#	LOCKED = 1
#	BACKEND_UNAVAILABLE = 2
#	BAD_ARGUMENTS = 3
#}

struct UnlockStatus {
	1: bool unlocked
	2: i16 more_keys_needed
}

exception InitException {
	1: InitError error
}

exception UnlockException {
	1: UnlockError error
}

exception RotateException {
	1: RotateError error
}

#exception RekeyException {
#	1: RekeyError error
#}
exception GetCardDataException {
	1: GetCardDataError error
}

exception PutCardDataException {
	1: PutCardDataError error
}


service Keyring {
	MasterKeyParts init (1: i16 threshold, 2: i16 num_parts) throws (1: InitException e)
	UnlockStatus unlock (1: MasterKeyPart key_part) throws (1: UnlockException e)
	void rotate () throws (1: RotateException e)
	#MasterKeyParts rekey (1: i16 threshold, 2: i16 num_parts) throws (1: RekeyException e)
}

service Storage {
	CardData getCardData (1: Token token) throws (1: GetCardDataException e)
	Token putCardData (1: CardData card_data) throws (1: PutCardDataException e)
}
