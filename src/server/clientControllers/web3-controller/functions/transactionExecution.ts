import { PaymasterMode } from '@biconomy/account';
import TokenContract from '../../../../utils/web3ContractData/token-address.json';

/**
 * Executes a transaction on the TokenContract using the provided transaction data.
 * @param txData The transaction data to be executed.
 * @param smartAccount The smart account used to send the transaction.
 * @returns An object indicating the success status and transaction hash of the executed transaction.
 */
export async function executeTransaction(txData: any, smartAccount: any) {
  try {
    // Construct the transaction object
    const tx = {
      to: TokenContract.address,
      data: txData,
    };

    // Send the transaction and get the transaction hash
    const userOpResponse = await smartAccount.sendTransaction(tx, {
      paymasterServiceData: { mode: PaymasterMode.SPONSORED },
    });
    const { transactionHash } = await userOpResponse.waitForTxHash();
    console.log('Transaction Hash', transactionHash);

    // Wait for the transaction to be mined and get the receipt
    const userOpReceipt = await userOpResponse.wait(5);
    if (userOpReceipt.success == 'true') {
      return { success: true, transactionHash };
    }
    // Return failure if the transaction was not successful
    return { success: false, transactionHash: '' };
  } catch (err) {
    // Throw an error if transaction execution fails
    throw new Error('Failed executing transaction');
  }
}
