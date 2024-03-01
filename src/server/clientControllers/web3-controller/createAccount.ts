import { ethers } from 'ethers';
import { createSmartAccountClient } from '@biconomy/account';

export const createSmartAccount = async () => {
  const provider = new ethers.providers.JsonRpcProvider(
    process.env.RPC_URL, // RPCURL
  );
  const signer = new ethers.Wallet(
    '1a9060d87234f853cad12f07c98c903ff8138f467a795f9faf0b7cfd48f52510', // Private key
    provider,
  );
  const biconomySmartAccountConfig: any = {
    signer: signer,
    bundlerUrl: process.env.BUNDLER_URL, // <-- Read about this at https://docs.biconomy.io/dashboard#bundler-url
    biconomyPaymasterApiKey: process.env.PAYMASTER_URL, // <-- Read about at https://docs.biconomy.io/dashboard/paymaster
  };

  const smartAccount = await createSmartAccountClient(
    biconomySmartAccountConfig,
  );

  const smartAccountAddress = await smartAccount.getAccountAddress();

  console.log('Smart Account Address', smartAccountAddress);
  return smartAccount;
};
