import { ethers } from 'ethers';
import TicketingContract from '../../../utils/web3ContractData/ticketing-contract-address.json';
import TicketingAbi from '../../../utils/web3ContractData/ticketing-contract.json';
import { PaymasterMode } from '@biconomy/account';
import { executeTransaction } from './functions/transaction';

export async function createWeb3Project(
  smartAccount: any,
  tokenURI: any,
  numOfTask: any,
) {
  try {
    let abi = [
      'function createProject(string memory _tokenURI,uint _numOfTask)',
    ];
    let iface = new ethers.utils.Interface(abi);
    const txData = iface.encodeFunctionData('createProject', [
      tokenURI,
      numOfTask, // number of tasks
    ]);
    await executeTransaction(txData, smartAccount);
  } catch (error) {
    console.log('Error in create project', error);
  }
}
