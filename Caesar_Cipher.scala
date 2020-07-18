//Caesar Cipher
//Index Number: 18000118

object Caesar_Cipher extends App{
	val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 			//alphabet which only contains uppercase letters

  	val encrypt = (st: Char, shift: Int, text: String) => {				//encryption function
    		if(text.contains(st))						//checks if the character "st" is in the given alphabet "text"
			text((st - text.head + shift + text.length) % text.length)		//shifts the character down the alphabet "shift" number of times

    		else{							//if the character "st" is not in the given alphabet "text"
			if (text.contains(st.toUpper))				//checks if the uppercase of character "st" is in the given alphabet "text"
				text((st - text.head.toLower + shift + text.length) % text.length).toLower	//shifts the character down the alphabet "shift" number of -
											//- times and converts the resultant character into lowercase
			else						//if the character "st" is neither uppercase nor lowercase ex: spaces
				st
    		}
  	}

  	val decrypt = (st: Char, shift: Int, text: String) => encrypt(st, -shift, text)		//since decryption is the opposite of encryption, "encrypt" function is -
									//- called with "-shift"

  	val cipher = (algo: (Char, Int, String) => Char, s: String, shift: Int, text: String) => s.map(algo(_, shift, text))		//cipher function which takes "encrypt" and -
												//- "decrypt" functions to process the message

  	val message = "University of Colombo School of Computing"		//message to be encrypted
  	val shift = 3						//number of times the alphabet should shift down
  	val encrypted = cipher(encrypt, message, shift, alphabet)		//cipher function takes "encrypt" to encrypt the message
	val decrypted = cipher(decrypt, encrypted, shift, alphabet)		//cipher function takes "decrypt" to decrypt the encrypted message

  	println("\nEntered message: " + message + "\nKey: " + shift)
  	println("\nEncrypted message: " + encrypted)
 	println("Decrypted message: " + decrypted)
}