import { ethers } from 'ethers';
import { executeTransaction } from './functions/transactionExecution';

// interface Web3TicketCreationData {
//   success: boolean;
//   transactionHash: string | null;
// }

/**
 * Creates a web3 ticket by minting a token using the provided smart account.
 * @param smartAccount The smart account used to execute the transaction.
 * @returns An object indicating the success status and transaction hash of the created ticket.
 */
export async function createWeb3Ticket(smartAccount: any) {
  try {
    // Define ABI for mintToken function
    const mintTokenABI = ['function mintToken(address to)'];
    const iface = new ethers.utils.Interface(mintTokenABI);
    const smartAccountAddress = await smartAccount.getAccountAddress();
    // Encode transaction data for mintToken function
    const txData = iface.encodeFunctionData('mintToken', [smartAccountAddress]);

    // Execute transaction and get transaction data
    const transactionData = await executeTransaction(txData, smartAccount);

    console.log('Transaction Data : ', transactionData);
    // Return success status and transaction hash
    if (transactionData?.success) {
      return {
        success: true,
        transactionHash: transactionData?.transactionHash,
      };
    } else {
      return {
        success: false,
        transactionHash: transactionData?.transactionHash,
      };
    }
  } catch (error) {
    // Log and throw error if transaction execution fails
    console.log('Error in creating web3 ticket:', error);
    throw new Error('Failed executing transaction');
  }
}
