import { PaymasterMode } from '@biconomy/account';
import TicketingContract from '../../../../utils/web3ContractData/ticketing-contract-address.json';

export async function executeTransaction(txData: any, smartAccount: any) {
  const tx = {
    to: TicketingContract.address,
    data: txData,
  };

  // Send the transaction and get the transaction hash
  const userOpResponse = await smartAccount.sendTransaction(tx, {
    paymasterServiceData: { mode: PaymasterMode.SPONSORED },
  });
  const { transactionHash } = await userOpResponse.waitForTxHash();
  console.log('Transaction Hash', transactionHash);
  const userOpReceipt = await userOpResponse.wait();
  if (userOpReceipt.success == 'true') {
    console.log('UserOp receipt', userOpReceipt);
    console.log('Transaction receipt', userOpReceipt.receipt);
  }
}
